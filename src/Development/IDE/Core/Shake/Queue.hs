-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ExplicitNamespaces         #-}

-- | The ShakeQueue which mediates communication between the server and
-- shake database.
module Development.IDE.Core.Shake.Queue(ShakeQueue(..)
                                       , newShakeQueue
                                       , DelayedAction(..)
                                       , mkDelayedAction
                                       , queueAction
                                       , getQueueWork
                                       , logDelayedAction
                                       , requeueIfCancelled
                                       ) where

import           Development.Shake hiding (ShakeValue, doesFileExist, Info)
import           Development.Shake.Classes
import           Data.Maybe
import qualified Data.Text as T
import Development.IDE.Core.Shake.Key
import Development.IDE.Types.Logger hiding (Priority)
import qualified Development.IDE.Types.Logger as Logger
import           Control.Concurrent.Extra
import           System.Time.Extra
import           Control.Monad.Extra
import qualified Data.HashPSQ as PQ


-- | A priority measure for an action in the queue
data QPriority = QPriority { retries :: Int
                           , qid :: Int
                           , qimportant :: Bool } deriving Eq

instance Ord QPriority where
    compare (QPriority r q i) (QPriority r' q' i') = compare i i' <> compare r r' <> compare q q'


data KeyWithId = KeyWithId Key Int deriving (Eq)

instance Hashable KeyWithId where
  hashWithSalt salt (KeyWithId k _) = hashWithSalt salt k

instance Ord KeyWithId where
    compare (KeyWithId h1 i1) (KeyWithId h2 i2) = i1 `compare` i2


type PriorityMap =  PQ.HashPSQ KeyWithId QPriority DelayedActionInternal

-- | Actions we want to run on the shake database are queued up and batched together.
-- A batch can be killed when a file is modified as we assume it will invalidate it.
data ShakeQueue = ShakeQueue
                { qactions :: Var PriorityMap
                -- An action which cancels the currently running batch and
                -- requeues the participants.
                , qabort :: Var (IO () -> IO ())
                -- A monotonically increasing integer to give each request
                -- a unique identifier.
                , qcount :: Var Int
                -- The worker takes this MVar when the actions are empty, it is filled
                -- when an action is written to the map which allows the
                -- worker to continue.
                , qTrigger :: MVar ()
                }

-- This is stuff we make up and add onto the information the user
-- provided.
data DelayedActionExtra = DelayedActionExtra { actionInternalId :: Int
                                             , actionInternalQPriority :: QPriority
                                             , actionInternalFinished :: IO Bool
                                             , actionInternal :: Action ()
                                             }

type DelayedAction a = DelayedActionX (Action a)
type DelayedActionInternal = DelayedActionX DelayedActionExtra

{-# COMPLETE DelayedActionInternal#-}
pattern DelayedActionInternal :: String -> Key -> Logger.Priority -> Action () -> Int -> QPriority -> IO Bool -> DelayedActionX DelayedActionExtra
pattern DelayedActionInternal {actionInternalName, actionInternalKey, actionInternalPriority, getAction
                              , actionId, actionQPriority, actionFinished}
                              = DelayedActionX actionInternalName actionInternalKey actionInternalPriority
                                    (DelayedActionExtra actionId actionQPriority actionFinished getAction)

{-# COMPLETE DelayedAction#-}
pattern DelayedAction :: String -> Key -> Logger.Priority -> Action a -> DelayedAction a
pattern DelayedAction a b c d = DelayedActionX a b c d

mkDelayedAction :: (Show k, Typeable k, Hashable k, Eq k)
                => String -> k -> Logger.Priority -> Action a -> DelayedAction a
mkDelayedAction s k = DelayedAction s (Key k)

data DelayedActionX a = DelayedActionX { actionName :: String -- Name we show to the user
                                      , actionKey :: Key
                                      -- The queue only contrains entries for
                                      -- unique key values.
                                      , actionPriority :: Logger.Priority
                                      -- An action which can be called to see
                                      -- if the action has finished yet.
                                      , actionExtra :: a
                                      }

instance Show (DelayedActionX a) where
    show d = "DelayedAction: " ++ actionName d

finishedBarrier :: Barrier a -> IO Bool
finishedBarrier b = isJust <$> waitBarrierMaybe b

freshId :: ShakeQueue -> IO Int
freshId (ShakeQueue{qcount}) = do
    modifyVar qcount (\n -> return (n + 1, n))

-- Could replace this argument with a parameterised version of
-- DelayedAction.
queueAction :: [DelayedAction a]
               -> ShakeQueue
               -> IO [Barrier a]
queueAction as sq = do
    (bs, ds) <- unzip <$> mapM (instantiateDelayedAction sq) as
    modifyVar_ (qactions sq) (return . insertMany ds)
    -- Wake up the worker if necessary
    void $ tryPutMVar (qTrigger sq) ()
    return bs

insertMany :: [DelayedActionInternal] -> PriorityMap -> PriorityMap
insertMany ds pm = foldr (\d pm' -> PQ.insert (mkKid d) (getPriority d) d pm') pm ds


mkKid :: DelayedActionX DelayedActionExtra -> KeyWithId
mkKid d = KeyWithId (actionKey d) (actionId d)

queueDelayedAction :: DelayedActionInternal -> ShakeQueue -> IO ()
queueDelayedAction d sq = do
    modifyVar_ (qactions sq) (return . PQ.insert (mkKid d) (getPriority d) d)
    -- Wake up the worker if necessary
    void $ tryPutMVar (qTrigger sq) ()

getPriority :: DelayedActionInternal -> QPriority
getPriority = actionQPriority

instantiateDelayedAction :: ShakeQueue -> DelayedAction a -> IO (Barrier a, DelayedActionInternal)
instantiateDelayedAction sq (DelayedAction s k p a) = do
    b <- newBarrier
    i <- freshId sq
    let a' = do r <- a
                liftIO $ signalBarrier b r
    let d = DelayedActionInternal s k p a' i (QPriority 0 i False) (finishedBarrier b)
    return (b, d)


newShakeQueue :: IO ShakeQueue
newShakeQueue = do
    ShakeQueue <$> newVar (PQ.empty) <*> (newVar id) <*> newVar 0 <*> newEmptyMVar

requeueIfCancelled :: ShakeQueue -> DelayedActionInternal -> IO ()
requeueIfCancelled sq d@(DelayedActionInternal{..}) = do
    is_finished <- actionFinished
    unless is_finished (queueDelayedAction d sq)

logDelayedAction :: Logger -> DelayedActionInternal -> Action ()
logDelayedAction l d  = do
    start <- liftIO $ offsetTime
    getAction d
    runTime <- liftIO $ start
    return ()
    liftIO $ logPriority l (actionPriority d) $ T.pack $
        "finish: " ++ (actionName d) ++ " (took " ++ showDuration runTime ++ ")"

-- | Retrieve up to k values from the map and return the modified map
smallestK :: Int -> PriorityMap -> (PriorityMap, [DelayedActionInternal])
smallestK 0 p = (p, [])
smallestK n p = case PQ.minView p of
                    Nothing -> (p, [])
                    Just (_, _, v, p') ->
                        let (p'', ds) = smallestK (n - 1) p'
                        in (p'', v:ds)

type QueueInfo = (Int, Int)

getWork :: Int -> PriorityMap -> (PriorityMap, (QueueInfo, [DelayedActionInternal]))
getWork n p =
    let before_size = PQ.size p
        (p', as) = smallestK n p
        after_size = PQ.size p'
    in (p', ((before_size, after_size), as))

getQueueWork :: ShakeQueue -> IO (QueueInfo, [DelayedActionInternal])
getQueueWork ShakeQueue{..} = modifyVar qactions (return . getWork 500)

