-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ConstraintKinds            #-}

-- | A Shake implementation of the compiler service.
--
--   There are two primary locations where data lives, and both of
--   these contain much the same data:
--
-- * The Shake database (inside 'shakeDb') stores a map of shake keys
--   to shake values. In our case, these are all of type 'Q' to 'A'.
--   During a single run all the values in the Shake database are consistent
--   so are used in conjunction with each other, e.g. in 'uses'.
--
-- * The 'Values' type stores a map of keys to values. These values are
--   always stored as real Haskell values, whereas Shake serialises all 'A' values
--   between runs. To deserialise a Shake value, we just consult Values.
module Development.IDE.Core.Shake(
    IdeState, shakeExtras,
    ShakeExtras(..), getShakeExtras, getShakeExtrasRules,
    IdeRule, IdeResult, GetModificationTime(..),
    shakeOpen, shakeShut,
    shakeRun, shakeRunInternal, shakeRunInternalKill, shakeRunUser,
    shakeProfile,
    use, useWithStale, useNoFile, uses, usesWithStale, useWithStaleFast, delayedAction,
    use_, useNoFile_, uses_,
    define, defineEarlyCutoff, defineOnDisk, needOnDisk, needOnDisks,
    getDiagnostics, unsafeClearDiagnostics,
    getHiddenDiagnostics,
    IsIdeGlobal, addIdeGlobal, addIdeGlobalExtras, getIdeGlobalState, getIdeGlobalAction,
    garbageCollect,
    knownFiles,
    setPriority,
    sendEvent,
    ideLogger,
    actionLogger,
    FileVersion(..), modificationTime, newerFileVersion,
    Priority(..),
    updatePositionMapping, runAction, runActionSync,
    deleteValue,
    OnDiskRule(..),

    workerThread, delay,
    IdeAction(..), runIdeAction
    ) where

import           Development.Shake hiding (ShakeValue, doesFileExist, Info)
import           Development.Shake.Database
import           Development.Shake.Classes
import           Development.Shake.Rule
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as BS
import           Data.Dynamic
import           Data.Maybe
import Data.Map.Strict (Map)
import           Data.List.Extra (foldl', partition, takeEnd)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Traversable (for)
import Data.Tuple.Extra
import Data.Unique
import Development.IDE.Core.Debouncer
import Development.IDE.Core.RuleTypes
import Development.IDE.Core.PositionMapping
import Development.IDE.Types.Logger hiding (Priority)
import qualified Development.IDE.Types.Logger as Logger
import Language.Haskell.LSP.Diagnostics
import qualified Data.SortedList as SL
import           Development.IDE.Types.Diagnostics
import Development.IDE.Types.Location
import Development.IDE.Types.Options
import           Control.Concurrent.Async
import           Control.Concurrent.Extra
import           Control.Exception
import           Control.DeepSeq
import           System.Time.Extra
import           Data.Typeable
import qualified Language.Haskell.LSP.Messages as LSP
import qualified Language.Haskell.LSP.Types as LSP
import           System.FilePath hiding (makeRelative)
import qualified Development.Shake as Shake
import           Control.Monad.Extra
import           Data.Time
import           GHC.Generics
import           System.IO.Unsafe
import           Numeric.Extra
import Language.Haskell.LSP.Types
import Data.Either.Extra
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.HashPSQ as PQ


-- information we stash inside the shakeExtra field
data ShakeExtras = ShakeExtras
    {eventer :: LSP.FromServerMessage -> IO ()
    ,debouncer :: Debouncer NormalizedUri
    ,logger :: Logger
    ,globals :: Var (HMap.HashMap TypeRep Dynamic)
    ,state :: Var Values
    ,diagnostics :: Var DiagnosticStore
    ,hiddenDiagnostics :: Var DiagnosticStore
    ,publishedDiagnostics :: Var (HMap.HashMap NormalizedUri [Diagnostic])
    -- ^ This represents the set of diagnostics that we have published.
    -- Due to debouncing not every change might get published.
    ,positionMapping :: Var (HMap.HashMap NormalizedUri (Map TextDocumentVersion PositionMapping))
    -- ^ Map from a text document version to a PositionMapping that describes how to map
    -- positions in a version of that document to positions in the latest version
    ,inProgress :: Var (HMap.HashMap NormalizedFilePath Int)
    -- ^ How many rules are running for each file
    , queue :: ShakeQueue
    }

getShakeExtras :: Action ShakeExtras
getShakeExtras = do
    Just x <- getShakeExtra @ShakeExtras
    return x

getShakeExtrasRules :: Rules ShakeExtras
getShakeExtrasRules = do
    Just x <- getShakeExtraRules @ShakeExtras
    return x

class Typeable a => IsIdeGlobal a where

addIdeGlobal :: IsIdeGlobal a => a -> Rules ()
addIdeGlobal x = do
    extras <- getShakeExtrasRules
    liftIO $ addIdeGlobalExtras extras x

addIdeGlobalExtras :: IsIdeGlobal a => ShakeExtras -> a -> IO ()
addIdeGlobalExtras ShakeExtras{globals} x@(typeOf -> ty) =
    liftIO $ modifyVar_ globals $ \mp -> case HMap.lookup ty mp of
        Just _ -> error $ "Can't addIdeGlobal twice on the same type, got " ++ show ty
        Nothing -> return $! HMap.insert ty (toDyn x) mp


getIdeGlobalExtras :: forall a . IsIdeGlobal a => ShakeExtras -> IO a
getIdeGlobalExtras ShakeExtras{globals} = do
    Just x <- HMap.lookup (typeRep (Proxy :: Proxy a)) <$> readVar globals
    return $ fromDyn x $ error "Serious error, corrupt globals"

getIdeGlobalAction :: forall a . IsIdeGlobal a => Action a
getIdeGlobalAction = liftIO . getIdeGlobalExtras =<< getShakeExtras

getIdeGlobalState :: forall a . IsIdeGlobal a => IdeState -> IO a
getIdeGlobalState = getIdeGlobalExtras . shakeExtras


-- | The state of the all values.
type Values = HMap.HashMap (NormalizedFilePath, Key) (Value Dynamic)

-- | Key type
data Key = forall k . (Typeable k, Hashable k, Eq k, Show k) => Key k

instance Show Key where
  show (Key k) = show k

instance Eq Key where
    Key k1 == Key k2 | Just k2' <- cast k2 = k1 == k2'
                     | otherwise = False

instance Hashable Key where
    hashWithSalt salt (Key key) = hashWithSalt salt key

-- | The result of an IDE operation. Warnings and errors are in the Diagnostic,
--   and a value is in the Maybe. For operations that throw an error you
--   expect a non-empty list of diagnostics, at least one of which is an error,
--   and a Nothing. For operations that succeed you expect perhaps some warnings
--   and a Just. For operations that depend on other failing operations you may
--   get empty diagnostics and a Nothing, to indicate this phase throws no fresh
--   errors but still failed.
--
--   A rule on a file should only return diagnostics for that given file. It should
--   not propagate diagnostic errors through multiple phases.
type IdeResult v = ([FileDiagnostic], Maybe v)

data Value v
    = Succeeded TextDocumentVersion v
    | Stale TextDocumentVersion v
    | Failed
    deriving (Functor, Generic, Show)

instance NFData v => NFData (Value v)

-- | Convert a Value to a Maybe. This will only return `Just` for
-- up2date results not for stale values.
currentValue :: Value v -> Maybe v
currentValue (Succeeded _ v) = Just v
currentValue (Stale _ _) = Nothing
currentValue Failed = Nothing

-- | Return the most recent, potentially stale, value and a PositionMapping
-- for the version of that value.
lastValueIO :: ShakeExtras -> NormalizedFilePath -> Value v -> IO (Maybe (v, PositionMapping))
lastValueIO ShakeExtras{positionMapping} file v = do
    allMappings <- liftIO $ readVar positionMapping
    pure $ case v of
        Succeeded ver v -> Just (v, mappingForVersion allMappings file ver)
        Stale ver v -> Just (v, mappingForVersion allMappings file ver)
        Failed -> Nothing

-- | Return the most recent, potentially stale, value and a PositionMapping
-- for the version of that value.
lastValue :: NormalizedFilePath -> Value v -> Action (Maybe (v, PositionMapping))
lastValue file v = do
    s <- getShakeExtras
    liftIO $ lastValueIO s file v

valueVersion :: Value v -> Maybe TextDocumentVersion
valueVersion = \case
    Succeeded ver _ -> Just ver
    Stale ver _ -> Just ver
    Failed -> Nothing

mappingForVersion
    :: HMap.HashMap NormalizedUri (Map TextDocumentVersion PositionMapping)
    -> NormalizedFilePath
    -> TextDocumentVersion
    -> PositionMapping
mappingForVersion allMappings file ver =
    fromMaybe idMapping $
    Map.lookup ver =<<
    HMap.lookup (filePathToUri' file) allMappings

type IdeRule k v =
  ( Shake.RuleResult k ~ v
  , Shake.ShakeValue k
  , Show v
  , Typeable v
  , NFData v
  )

-- | A Shake database plus persistent store. Can be thought of as storing
--   mappings from @(FilePath, k)@ to @RuleResult k@.
data IdeState = IdeState
    {shakeDb :: ShakeDatabase
    ,shakeAbort :: MVar (IO ()) -- close whoever was running last
    ,shakeClose :: IO ()
    ,shakeQueue :: ShakeQueue
    ,shakeExtras :: ShakeExtras
    ,shakeProfileDir :: Maybe FilePath
    }


data QPriority = QPriority { retries :: Int
                           , qid :: Int
                           , qimportant :: Bool } deriving Eq

instance Ord QPriority where
    compare (QPriority r q i) (QPriority r' q' i') = compare i i' <> compare r r' <> compare q q'


type PriorityMap = PQ.HashPSQ Int QPriority DelayedAction

-- | Actions we want to run on the shake database are queued up and batched together.
-- A batch can be killed when a file is modified as we assume it will invalidate it.
data ShakeQueue = ShakeQueue
                { qactions :: Var PriorityMap
                , qabort :: Var (IO () -> IO ())
                , qcount :: Var Int
                -- We take this MVar when the actions are empty, it is filled
                -- when an action is written to the map
                , qTrigger :: MVar ()
                }

data DelayedAction = DelayedAction { actionName :: String
                                   , actionId :: Int
                                   , actionQPriority :: QPriority
                                   , actionPriority :: Logger.Priority
                                   , actionFinished :: (IO Bool)
                                   , getAction :: (Action ()) }

instance Show DelayedAction where
    show d = "DelayedAction: " ++ actionName d

finishedBarrier :: Barrier a -> IO Bool
finishedBarrier b = isJust <$> waitBarrierMaybe b

freshId :: ShakeQueue -> IO Int
freshId (ShakeQueue{qcount}) = do
    modifyVar qcount (\n -> return (n + 1, n))

queueAction :: String -> Logger.Priority -> [Action a] -> ShakeQueue -> IO [Barrier a]
queueAction s p as sq = do
    (bs, ds) <- unzip <$> mapM (mkDelayedAction sq s p) as
    modifyVar_ (qactions sq) (return . insertMany ds)
    -- Wake up the worker if necessary
    void $ tryPutMVar (qTrigger sq) ()
    return bs

insertMany :: [DelayedAction] -> PriorityMap -> PriorityMap
insertMany ds pm = foldr (\d pm' -> PQ.insert (actionId d) (getPriority d) d pm') pm ds

queueDelayedAction :: DelayedAction -> ShakeQueue -> IO ()
queueDelayedAction d sq = do
    modifyVar_ (qactions sq) (return . PQ.insert (actionId d) (getPriority d) d)
    -- Wake up the worker if necessary
    void $ tryPutMVar (qTrigger sq) ()

getPriority :: DelayedAction -> QPriority
getPriority DelayedAction{..} = actionQPriority

mkDelayedAction :: ShakeQueue -> String -> Logger.Priority -> Action a -> IO (Barrier a, DelayedAction)
mkDelayedAction sq s p a = do
    b <- newBarrier
    i <- freshId sq
    let d = DelayedAction s i (QPriority 0 i False) p (finishedBarrier b)
                  (do r <- a
                      liftIO $ signalBarrier b r)
    return (b, d)


newShakeQueue :: IO ShakeQueue
newShakeQueue = do
    ShakeQueue <$> newVar (PQ.empty) <*> (newVar id) <*> newVar 0 <*> newEmptyMVar

requeueIfCancelled :: ShakeQueue -> DelayedAction -> IO ()
requeueIfCancelled sq d@(DelayedAction{..}) = do
    is_finished <- actionFinished
    unless is_finished (queueDelayedAction d sq)

logDelayedAction :: Logger -> DelayedAction -> Action ()
logDelayedAction l d  = do
    start <- liftIO $ offsetTime
    getAction d
    runTime <- liftIO $ start
    return ()
    liftIO $ logPriority l (actionPriority d) $ T.pack $
        "finish: " ++ (actionName d) ++ " (took " ++ showDuration runTime ++ ")"

-- | Retrieve up to k values from the map and return the modified map
smallestK :: Int -> PriorityMap -> (PriorityMap, [DelayedAction])
smallestK 0 p = (p, [])
smallestK n p = case PQ.minView p of
                    Nothing -> (p, [])
                    Just (_, _, v, p') ->
                        let (p'', ds) = smallestK (n - 1) p'
                        in (p'', v:ds)



-- | A thread which continually reads from the queue running shake actions
workerThread :: IdeState -> IO ()
workerThread i@IdeState{shakeQueue=sq@ShakeQueue{..},..} = do
    -- I choose 5 here but may be better to just chuck the whole thing to shake as it will paralellise the work itself.
    ds <- modifyVar qactions (return . smallestK 5)
    case ds of
        [] -> takeMVar qTrigger
        _ -> do
            logDebug (logger shakeExtras) (T.pack $ "Starting: " ++ show ds)
            (cancel, wait) <- shakeRun Debug "batch" i (map (logDelayedAction (logger shakeExtras)) ds)
            writeVar qabort (\k -> do
                k
                cancel
                mapM_ (requeueIfCancelled sq) ds)
            res <- try wait
            case res of
                -- Really don't want an exception to kill this thread but not sure where it should go
                Left (e :: SomeException) -> return ()
                Right r -> return ()
            -- Action finished, nothing to abort now
            writeVar qabort id
            return ()



-- This is debugging code that generates a series of profiles, if the Boolean is true
shakeRunDatabaseProfile :: Maybe FilePath -> ShakeDatabase -> [Action a] -> IO (([a], [IO ()]), Maybe FilePath)
shakeRunDatabaseProfile mbProfileDir shakeDb acts = do
        (time, res) <- duration $ shakeRunDatabase shakeDb acts
        proFile <- for mbProfileDir $ \dir -> do
                count <- modifyVar profileCounter $ \x -> let !y = x+1 in return (y,y)
                let file = "ide-" ++ profileStartTime ++ "-" ++ takeEnd 5 ("0000" ++ show count) ++ "-" ++ showDP 2 time <.> "html"
                shakeProfileDatabase shakeDb $ dir </> file
                return (dir </> file)
        return (res, proFile)

-- Write a profile of the last action to be completed
shakeWriteProfile :: String -> ShakeDatabase -> Seconds -> FilePath -> IO FilePath
shakeWriteProfile herald shakeDb time dir = do
     count <- modifyVar profileCounter $ \x -> let !y = x+1 in return (y,y)
     let file = herald ++ "-ide-" ++ profileStartTime ++ "-" ++ takeEnd 5 ("0000" ++ show count) ++ "-" ++ showDP 2 time <.> "html"
     shakeProfileDatabase shakeDb $ dir </> file
     return (dir </> file)

{-# NOINLINE profileStartTime #-}
profileStartTime :: String
profileStartTime = unsafePerformIO $ formatTime defaultTimeLocale "%Y%m%d-%H%M%S" <$> getCurrentTime

{-# NOINLINE profileCounter #-}
profileCounter :: Var Int
profileCounter = unsafePerformIO $ newVar 0

setValues :: IdeRule k v
          => Var Values
          -> k
          -> NormalizedFilePath
          -> Value v
          -> IO ()
setValues state key file val = modifyVar_ state $ \vals -> do
    -- Force to make sure the old HashMap is not retained
    evaluate $ HMap.insert (file, Key key) (fmap toDyn val) vals

-- | Delete the value stored for a given ide build key
deleteValue
  :: (Typeable k, Hashable k, Eq k, Show k)
  => IdeState
  -> k
  -> NormalizedFilePath
  -> IO ()
deleteValue IdeState{shakeExtras = ShakeExtras{state}} key file = modifyVar_ state $ \vals ->
    evaluate $ HMap.delete (file, Key key) vals

-- | We return Nothing if the rule has not run and Just Failed if it has failed to produce a value.
getValues :: forall k v. IdeRule k v => Var Values -> k -> NormalizedFilePath -> IO (Maybe (Value v))
getValues state key file = do
    vs <- readVar state
    case HMap.lookup (file, Key key) vs of
        Nothing -> pure Nothing
        Just v -> do
            let r = fmap (fromJust . fromDynamic @v) v
            -- Force to make sure we do not retain a reference to the HashMap
            -- and we blow up immediately if the fromJust should fail
            -- (which would be an internal error).
            evaluate (r `seqValue` Just r)

-- Consult the Values hashmap to get a list of all the files we care about
-- in a project
-- MP: This may be quite inefficient if the Values table is very big but
-- simplest implementation first.
knownFilesIO :: Var Values -> IO (HSet.HashSet NormalizedFilePath)
knownFilesIO v = do
  vs <- readVar v
  return $ HSet.map fst $ HSet.filter (\(_, k) -> k == Key GetHiFile) (HMap.keysSet vs)

knownFiles :: Action (HSet.HashSet NormalizedFilePath)
knownFiles = do
  ShakeExtras{state} <- getShakeExtras
  liftIO $ knownFilesIO state




-- | Seq the result stored in the Shake value. This only
-- evaluates the value to WHNF not NF. We take care of the latter
-- elsewhere and doing it twice is expensive.
seqValue :: Value v -> b -> b
seqValue v b = case v of
    Succeeded ver v -> rnf ver `seq` v `seq` b
    Stale ver v -> rnf ver `seq` v `seq` b
    Failed -> b

-- | Open a 'IdeState', should be shut using 'shakeShut'.
shakeOpen :: IO LSP.LspId
          -> (LSP.FromServerMessage -> IO ()) -- ^ diagnostic handler
          -> Logger
          -> Debouncer NormalizedUri
          -> Maybe FilePath
          -> IdeReportProgress
          -> ShakeOptions
          -> Rules ()
          -> IO IdeState
shakeOpen getLspId eventer logger debouncer shakeProfileDir (IdeReportProgress reportProgress) opts rules = do
    inProgress <- newVar HMap.empty
    shakeAbort <- newMVar $ return ()
    shakeQueue <- newShakeQueue
    shakeExtras <- do
        globals <- newVar HMap.empty
        state <- newVar HMap.empty
        diagnostics <- newVar mempty
        hiddenDiagnostics <- newVar mempty
        publishedDiagnostics <- newVar mempty
        positionMapping <- newVar HMap.empty
        let queue = shakeQueue
        pure ShakeExtras{..}
    (shakeDb, shakeClose) <-
        shakeOpenDatabase
            opts
                { shakeExtra = addShakeExtra shakeExtras $ shakeExtra opts
                -- we don't actually use the progress value, but Shake conveniently spawns/kills this thread whenever
                -- we call into Shake, so abuse it for that purpose
                , shakeProgress = const $ if reportProgress then lspShakeProgress getLspId eventer inProgress else pure ()
                }
            rules
    shakeDb <- shakeDb
    return IdeState{..}

lspShakeProgress :: Hashable a => IO LSP.LspId -> (LSP.FromServerMessage -> IO ()) -> Var (HMap.HashMap a Int) -> IO ()
lspShakeProgress getLspId sendMsg inProgress = do
    -- first sleep a bit, so we only show progress messages if it's going to take
    -- a "noticable amount of time" (we often expect a thread kill to arrive before the sleep finishes)
    sleep 0.1
    lspId <- getLspId
    u <- ProgressTextToken . T.pack . show . hashUnique <$> newUnique
    sendMsg $ LSP.ReqWorkDoneProgressCreate $ LSP.fmServerWorkDoneProgressCreateRequest
      lspId $ LSP.WorkDoneProgressCreateParams
      { _token = u }
    bracket_ (start u) (stop u) (loop u Nothing)
    where
        start id = sendMsg $ LSP.NotWorkDoneProgressBegin $ LSP.fmServerWorkDoneProgressBeginNotification
            LSP.ProgressParams
                { _token = id
                , _value = WorkDoneProgressBeginParams
                  { _title = "Processing"
                  , _cancellable = Nothing
                  , _message = Nothing
                  , _percentage = Nothing
                  }
                }
        stop id = sendMsg $ LSP.NotWorkDoneProgressEnd $ LSP.fmServerWorkDoneProgressEndNotification
            LSP.ProgressParams
                { _token = id
                , _value = WorkDoneProgressEndParams
                  { _message = Nothing
                  }
                }
        sample = 0.1
        loop id prev = do
            sleep sample
            current <- readVar inProgress
            let done = length $ filter (== 0) $ HMap.elems current
            let todo = HMap.size current
            let next = Just $ T.pack $ show done <> "/" <> show todo
            when (next /= prev) $
                sendMsg $ LSP.NotWorkDoneProgressReport $ LSP.fmServerWorkDoneProgressReportNotification
                    LSP.ProgressParams
                        { _token = id
                        , _value = LSP.WorkDoneProgressReportParams
                        { _cancellable = Nothing
                        , _message = next
                        , _percentage = Nothing
                        }
                        }
            loop id next

shakeProfile :: IdeState -> FilePath -> IO ()
shakeProfile IdeState{..} = shakeProfileDatabase shakeDb

shakeShut :: IdeState -> IO ()
shakeShut IdeState{..} = withMVar shakeAbort $ \stop -> do
    -- Shake gets unhappy if you try to close when there is a running
    -- request so we first abort that.
    stop
    shakeClose

-- | This is a variant of withMVar where the first argument is run unmasked and if it throws
-- an exception, the previous value is restored while the second argument is executed masked.
withMVar' :: MVar a -> (a -> IO ()) -> IO (a, c) -> IO c
withMVar' var unmasked masked = mask $ \restore -> do
    a <- takeMVar var
    restore (unmasked a) `onException` putMVar var a
    (a', c) <- masked
    putMVar var a'
    pure c

-- | These actions are run asynchronously after the current action is
-- finished running. For example, to trigger a key build after a rule
-- has already finished as is the case with useWithStaleFast
delayedAction :: String -> Action () -> IdeAction ()
delayedAction herald a = tell [(herald, a)]

-- | A varient of delayedAction for the Action monad
-- The supplied action *will* be run but at least not until the current action has finished.
delay :: String -> Action () -> Action ()
delay herald a = do
    ShakeExtras{queue} <- getShakeExtras
    -- Do not wait for the action to return
    void $ liftIO $ queueAction herald Info [a] queue

--  runAfterDB (\db -> do
--    void $ shakeRun Debug ("DELAYED:" ++ herald) s [a] db)

-- | Running an action which DIRECTLY corresponds to something a user did,
-- for example computing hover information.
shakeRunUser :: String -> IdeState -> [Action a] -> IO ([IO a])
shakeRunUser s ide as = do
    bs <- queueAction s Info as (shakeQueue ide)
    return $ map waitBarrier bs
-- | Running an action which is INTERNAL to shake, for example, a file has
-- been modified, basically any use of shakeRun which is not blocking (ie
-- which is not called via runAction).
shakeRunInternal :: String -> IdeState -> [Action a] -> IO ()
shakeRunInternal s ide as = void $ queueAction s Debug as (shakeQueue ide)

-- | Like shakeRun internal but kill ongoing work first
shakeRunInternalKill :: String -> IdeState -> [Action a] -> IO ()
shakeRunInternalKill s ide as = do
    -- TODO: There is a race here if the actions get queued
    -- and start to run before the kill, sh
    let k = shakeRunInternal s ide as
    kill <- readVar (qabort (shakeQueue ide))
    kill k


-- | Spawn immediately. If you are already inside a call to shakeRun that will be aborted with an exception.
shakeRun :: Logger.Priority
         -> String  -- A name for the action and at which logging priority it should be displayed.
                                -- Some requests are very internal to shake
                                -- and don't make much sense to normal
                                -- users.
         -> IdeState -> [Action a] -> IO (IO (), IO [a])
shakeRun p herald IdeState{shakeExtras=ShakeExtras{..},..} acts  =
    {-
    withMVar'
        abort
        (\stop -> do
              (stopTime,_) <- duration stop
              return ()
              logPriority logger p $ T.pack $ "start shakeRun: " ++ herald ++ " (abort time:" ++ showDuration stopTime ++ ")"
        )
        -- It is crucial to be masked here, otherwise we can get killed
        -- between spawning the new thread and updating shakeAbort.
        -- See https://github.com/digital-asset/ghcide/issues/79
        -}
        (do
              aThread <- asyncWithUnmask $ \restore -> do
                    -- Try to run the action and record how long it took
                   (runTime, res) <- duration $ try (restore $ shakeRunDatabase shakeDb acts)

                   -- Write a profile when the action completed normally
                   profile <- case res of
                     Left {}  -> return ""
                     Right {} -> do
                      mfp <- forM shakeProfileDir (shakeWriteProfile herald shakeDb runTime)
                      case mfp of
                        Just fp ->  return $
                          let link = case filePathToUri' $ toNormalizedFilePath' fp of
                                                NormalizedUri _ x -> x
                          in ", profile saved at " <> T.unpack link
                        Nothing -> return ""

                   let res' = case res of
                            Left e -> "exception: " <> displayException e
                            Right _ -> "completed"
                   logPriority logger p $ T.pack $
                        "finish shakeRun: " ++ herald ++ " (took " ++ showDuration runTime ++ ", " ++ res' ++ profile ++ ")"
                   return res
              let wrapUp res = either (throwIO @SomeException) return res
              -- An action which can be used to block on the result of
              -- shakeRun returning
              let k = do
                        (res, as) <- (wrapUp =<< wait aThread)
                        sequence_ as
                        return res
              pure (cancel aThread, k))

getDiagnostics :: IdeState -> IO [FileDiagnostic]
getDiagnostics IdeState{shakeExtras = ShakeExtras{diagnostics}} = do
    val <- readVar diagnostics
    return $ getAllDiagnostics val

getHiddenDiagnostics :: IdeState -> IO [FileDiagnostic]
getHiddenDiagnostics IdeState{shakeExtras = ShakeExtras{hiddenDiagnostics}} = do
    val <- readVar hiddenDiagnostics
    return $ getAllDiagnostics val

-- | FIXME: This function is temporary! Only required because the files of interest doesn't work
unsafeClearDiagnostics :: IdeState -> IO ()
unsafeClearDiagnostics IdeState{shakeExtras = ShakeExtras{diagnostics}} =
    writeVar diagnostics mempty

-- | Clear the results for all files that do not match the given predicate.
garbageCollect :: (NormalizedFilePath -> Bool) -> Action ()
garbageCollect keep = do
    ShakeExtras{state, diagnostics,hiddenDiagnostics,publishedDiagnostics,positionMapping} <- getShakeExtras
    liftIO $
        do newState <- modifyVar state $ \values -> do
               values <- evaluate $ HMap.filterWithKey (\(file, _) _ -> keep file) values
               return $! dupe values
           modifyVar_ diagnostics $ \diags -> return $! filterDiagnostics keep diags
           modifyVar_ hiddenDiagnostics $ \hdiags -> return $! filterDiagnostics keep hdiags
           modifyVar_ publishedDiagnostics $ \diags -> return $! HMap.filterWithKey (\uri _ -> keep (fromUri uri)) diags
           let versionsForFile =
                   HMap.fromListWith Set.union $
                   mapMaybe (\((file, _key), v) -> (filePathToUri' file,) . Set.singleton <$> valueVersion v) $
                   HMap.toList newState
           modifyVar_ positionMapping $ \mappings -> return $! filterVersionMap versionsForFile mappings
define
    :: IdeRule k v
    => (k -> NormalizedFilePath -> Action (IdeResult v)) -> Rules ()
define op = defineEarlyCutoff $ \k v -> (Nothing,) <$> op k v

use :: IdeRule k v
    => k -> NormalizedFilePath -> Action (Maybe v)
use key file = head <$> uses key [file]

useWithStale :: IdeRule k v
    => k -> NormalizedFilePath -> Action (Maybe (v, PositionMapping))
useWithStale key file = head <$> usesWithStale key [file]

newtype IdeAction a = IdeAction { runIdeActionT  :: WriterT [(String, Action ())] (ReaderT IdeState IO) a }
    deriving (MonadReader IdeState, MonadWriter [(String, Action ())], MonadIO, Functor, Applicative, Monad)

runIdeAction :: String -> IdeState -> IdeAction a -> IO a
runIdeAction _herald s i = do
    (res, ws) <- runReaderT (runWriterT (runIdeActionT i)) s
    mapM_ (\(herald2, a) -> shakeRunInternal herald2 s [a]) ws
    return res

askShake :: IdeAction ShakeExtras
askShake = shakeExtras <$> ask

useWithStaleFast :: IdeRule k v => k -> NormalizedFilePath -> IdeAction (Maybe (v, PositionMapping))
useWithStaleFast key file = do
  final_res <-  do
    -- This lookup directly looks up the key in the shake database and
    -- returns the last value that was computed for this key without
    -- checking freshness.

    s@ShakeExtras{state} <- askShake
    r <- liftIO $ getValues state key file
    case r of
      Nothing -> do
        -- Perhaps for Hover this should return Nothing immediatey but for
        -- completions it should block? Not for MP to decide, need AZ and
        -- F to comment
        return Nothing
        --useWithStale key file
      -- Otherwise, use the computed value even if it's out of date.
      Just v -> do
        liftIO $ lastValueIO s file v
  -- Then async trigger the key to be built anyway because we want to
  -- keep updating the value in the key.
  --shakeRunInternal ("C:" ++ (show key)) ide [use key file]
  delayedAction ("C:" ++ (show key)) (void $ use key file)
  return final_res

-- MattP: Removed the distinction between runAction and runActionSync
-- as it is no longer necessary now the are no `action` rules. This is how
-- it should remain as they add a lot of overhead for hover for example if
-- you run them every time.
runAction :: String -> IdeState -> Action a -> IO a
runAction herald ide action = runActionSync herald ide action

runActionSync :: String -> IdeState -> Action a -> IO a
runActionSync herald s act = fmap head $ join $ sequence <$> shakeRunUser herald s [act]

useNoFile :: IdeRule k v => k -> Action (Maybe v)
useNoFile key = use key emptyFilePath

use_ :: IdeRule k v => k -> NormalizedFilePath -> Action v
use_ key file = head <$> uses_ key [file]

useNoFile_ :: IdeRule k v => k -> Action v
useNoFile_ key = use_ key emptyFilePath

uses_ :: IdeRule k v => k -> [NormalizedFilePath] -> Action [v]
uses_ key files = do
    res <- uses key files
    case sequence res of
        Nothing -> liftIO $ throwIO $ BadDependency (show key)
        Just v -> return v


-- | When we depend on something that reported an error, and we fail as a direct result, throw BadDependency
--   which short-circuits the rest of the action
data BadDependency = BadDependency String deriving Show
instance Exception BadDependency

isBadDependency :: SomeException -> Bool
isBadDependency x
    | Just (x :: ShakeException) <- fromException x = isBadDependency $ shakeExceptionInner x
    | Just (_ :: BadDependency) <- fromException x = True
    | otherwise = False

newtype Q k = Q (k, NormalizedFilePath)
    deriving (Eq,Hashable,NFData, Generic)

instance Binary k => Binary (Q k)

instance Show k => Show (Q k) where
    show (Q (k, file)) = show k ++ "; " ++ fromNormalizedFilePath file

-- | Invariant: the 'v' must be in normal form (fully evaluated).
--   Otherwise we keep repeatedly 'rnf'ing values taken from the Shake database
-- Note (MK) I am not sure why we need the ShakeValue here, maybe we
-- can just remove it?
data A v = A (Value v) ShakeValue
    deriving Show

instance NFData (A v) where rnf (A v x) = v `seq` rnf x

-- In the Shake database we only store one type of key/result pairs,
-- namely Q (question) / A (answer).
type instance RuleResult (Q k) = A (RuleResult k)


-- | Return up2date results. Stale results will be ignored.
uses :: IdeRule k v
    => k -> [NormalizedFilePath] -> Action [Maybe v]
uses key files = map (\(A value _) -> currentValue value) <$> apply (map (Q . (key,)) files)

-- | Return the last computed result which might be stale.
usesWithStale :: IdeRule k v
    => k -> [NormalizedFilePath] -> Action [Maybe (v, PositionMapping)]
usesWithStale key files = do
    values <- map (\(A value _) -> value) <$> apply (map (Q . (key,)) files)
    zipWithM lastValue files values


withProgress :: (Eq a, Hashable a) => Var (HMap.HashMap a Int) -> a -> Action b -> Action b
withProgress var file = actionBracket (f succ) (const $ f pred) . const
    where f shift = modifyVar_ var $ \x -> return (HMap.alter (Just . shift . fromMaybe 0) file x)


defineEarlyCutoff
    :: IdeRule k v
    => (k -> NormalizedFilePath -> Action (Maybe BS.ByteString, IdeResult v))
    -> Rules ()
defineEarlyCutoff op = addBuiltinRule noLint noIdentity $ \(Q (key, file)) (old :: Maybe BS.ByteString) mode -> do
    extras@ShakeExtras{state, inProgress} <- getShakeExtras
    -- don't do progress for GetFileExists, as there are lots of non-nodes for just that one key
    (if show key == "GetFileExists" then id else withProgress inProgress file) $ do
        val <- case old of
            Just old | mode == RunDependenciesSame -> do
                v <- liftIO $ getValues state key file
                case v of
                    -- No changes in the dependencies and we have
                    -- an existing result.
                    Just v -> return $ Just $ RunResult ChangedNothing old $ A v (decodeShakeValue old)
                    _ -> return Nothing
            _ -> return Nothing
        case val of
            Just res -> return res
            Nothing -> do
                (bs, (diags, res)) <- actionCatch
                    (do v <- op key file; liftIO $ evaluate $ force v) $
                    \(e :: SomeException) -> pure (Nothing, ([ideErrorText file $ T.pack $ show e | not $ isBadDependency e],Nothing))
                modTime <- liftIO $ (currentValue =<<) <$> getValues state GetModificationTime file
                (bs, res) <- case res of
                    Nothing -> do
                        staleV <- liftIO $ getValues state key file
                        pure $ case staleV of
                            Nothing -> (toShakeValue ShakeResult bs, Failed)
                            Just v -> case v of
                                Succeeded ver v -> (toShakeValue ShakeStale bs, Stale ver v)
                                Stale ver v -> (toShakeValue ShakeStale bs, Stale ver v)
                                Failed -> (toShakeValue ShakeResult bs, Failed)
                    Just v -> pure (maybe ShakeNoCutoff ShakeResult bs, Succeeded (vfsVersion =<< modTime) v)
                liftIO $ setValues state key file res
                updateFileDiagnostics file (Key key) extras $ map (\(_,y,z) -> (y,z)) diags
                let eq = case (bs, fmap decodeShakeValue old) of
                        (ShakeResult a, Just (ShakeResult b)) -> a == b
                        (ShakeStale a, Just (ShakeStale b)) -> a == b
                        -- If we do not have a previous result
                        -- or we got ShakeNoCutoff we always return False.
                        _ -> False
                return $ RunResult
                    (if eq then ChangedRecomputeSame else ChangedRecomputeDiff)
                    (encodeShakeValue bs) $
                    A res bs


-- | Rule type, input file
data QDisk k = QDisk k NormalizedFilePath
  deriving (Eq, Generic)

instance Hashable k => Hashable (QDisk k)

instance NFData k => NFData (QDisk k)

instance Binary k => Binary (QDisk k)

instance Show k => Show (QDisk k) where
    show (QDisk k file) =
        show k ++ "; " ++ fromNormalizedFilePath file

type instance RuleResult (QDisk k) = Bool

data OnDiskRule = OnDiskRule
  { getHash :: Action BS.ByteString
  -- This is used to figure out if the state on disk corresponds to the state in the Shake
  -- database and we can therefore avoid rerunning. Often this can just be the file hash but
  -- in some cases we can be more aggressive, e.g., for GHC interface files this can be the ABI hash which
  -- is more stable than the hash of the interface file.
  -- An empty bytestring indicates that the state on disk is invalid, e.g., files are missing.
  -- We do not use a Maybe since we have to deal with encoding things into a ByteString anyway in the Shake DB.
  , runRule :: Action (IdeResult BS.ByteString)
  -- The actual rule code which produces the new hash (or Nothing if the rule failed) and the diagnostics.
  }

-- This is used by the DAML compiler for incremental builds. Right now this is not used by
-- ghcide itself but that might change in the future.
-- The reason why this code lives in ghcide and in particular in this module is that it depends quite heavily on
-- the internals of this module that we do not want to expose.
defineOnDisk
  :: (Shake.ShakeValue k, RuleResult k ~ ())
  => (k -> NormalizedFilePath -> OnDiskRule)
  -> Rules ()
defineOnDisk act = addBuiltinRule noLint noIdentity $
  \(QDisk key file) (mbOld :: Maybe BS.ByteString) mode -> do
      extras <- getShakeExtras
      let OnDiskRule{..} = act key file
      let validateHash h
              | BS.null h = Nothing
              | otherwise = Just h
      let runAct = actionCatch runRule $
              \(e :: SomeException) -> pure ([ideErrorText file $ T.pack $ displayException e | not $ isBadDependency e], Nothing)
      case mbOld of
          Nothing -> do
              (diags, mbHash) <- runAct
              updateFileDiagnostics file (Key key) extras $ map (\(_,y,z) -> (y,z)) diags
              pure $ RunResult ChangedRecomputeDiff (fromMaybe "" mbHash) (isJust mbHash)
          Just old -> do
              current <- validateHash <$> (actionCatch getHash $ \(_ :: SomeException) -> pure "")
              if mode == RunDependenciesSame && Just old == current && not (BS.null old)
                  then
                    -- None of our dependencies changed, we’ve had a successful run before and
                    -- the state on disk matches the state in the Shake database.
                    pure $ RunResult ChangedNothing (fromMaybe "" current) (isJust current)
                  else do
                    (diags, mbHash) <- runAct
                    updateFileDiagnostics file (Key key) extras $ map (\(_,y,z) -> (y,z)) diags
                    let change
                          | mbHash == Just old = ChangedRecomputeSame
                          | otherwise = ChangedRecomputeDiff
                    pure $ RunResult change (fromMaybe "" mbHash) (isJust mbHash)

needOnDisk :: (Shake.ShakeValue k, RuleResult k ~ ()) => k -> NormalizedFilePath -> Action ()
needOnDisk k file = do
    successfull <- apply1 (QDisk k file)
    liftIO $ unless successfull $ throwIO $ BadDependency (show k)

needOnDisks :: (Shake.ShakeValue k, RuleResult k ~ ()) => k -> [NormalizedFilePath] -> Action ()
needOnDisks k files = do
    successfulls <- apply $ map (QDisk k) files
    liftIO $ unless (and successfulls) $ throwIO $ BadDependency (show k)

toShakeValue :: (BS.ByteString -> ShakeValue) -> Maybe BS.ByteString -> ShakeValue
toShakeValue = maybe ShakeNoCutoff

data ShakeValue
    = ShakeNoCutoff
    -- ^ This is what we use when we get Nothing from
    -- a rule.
    | ShakeResult !BS.ByteString
    -- ^ This is used both for `Failed`
    -- as well as `Succeeded`.
    | ShakeStale !BS.ByteString
    deriving (Generic, Show)

instance NFData ShakeValue

encodeShakeValue :: ShakeValue -> BS.ByteString
encodeShakeValue = \case
    ShakeNoCutoff -> BS.empty
    ShakeResult r -> BS.cons 'r' r
    ShakeStale r -> BS.cons 's' r

decodeShakeValue :: BS.ByteString -> ShakeValue
decodeShakeValue bs = case BS.uncons bs of
    Nothing -> ShakeNoCutoff
    Just (x, xs)
      | x == 'r' -> ShakeResult xs
      | x == 's' -> ShakeStale xs
      | otherwise -> error $ "Failed to parse shake value " <> show bs


updateFileDiagnostics :: MonadIO m
  => NormalizedFilePath
  -> Key
  -> ShakeExtras
  -> [(ShowDiagnostic,Diagnostic)] -- ^ current results
  -> m ()
updateFileDiagnostics fp k ShakeExtras{diagnostics, hiddenDiagnostics, publishedDiagnostics, state, debouncer, eventer} current = liftIO $ do
    modTime <- (currentValue =<<) <$> getValues state GetModificationTime fp
    let (currentShown, currentHidden) = partition ((== ShowDiag) . fst) current
    mask_ $ do
        -- Mask async exceptions to ensure that updated diagnostics are always
        -- published. Otherwise, we might never publish certain diagnostics if
        -- an exception strikes between modifyVar but before
        -- publishDiagnosticsNotification.
        newDiags <- modifyVar diagnostics $ \old -> do
            let newDiagsStore = setStageDiagnostics fp (vfsVersion =<< modTime)
                                  (T.pack $ show k) (map snd currentShown) old
            let newDiags = getFileDiagnostics fp newDiagsStore
            _ <- evaluate newDiagsStore
            _ <- evaluate newDiags
            pure (newDiagsStore, newDiags)
        modifyVar_ hiddenDiagnostics $ \old -> do
            let newDiagsStore = setStageDiagnostics fp (vfsVersion =<< modTime)
                                  (T.pack $ show k) (map snd currentHidden) old
            let newDiags = getFileDiagnostics fp newDiagsStore
            _ <- evaluate newDiagsStore
            _ <- evaluate newDiags
            return newDiagsStore
        let uri = filePathToUri' fp
        let delay = if null newDiags then 0.1 else 0
        registerEvent debouncer delay uri $ do
             mask_ $ modifyVar_ publishedDiagnostics $ \published -> do
                 let lastPublish = HMap.lookupDefault [] uri published
                 when (lastPublish /= newDiags) $
                     eventer $ publishDiagnosticsNotification (fromNormalizedUri uri) newDiags
                 pure $! HMap.insert uri newDiags published

publishDiagnosticsNotification :: Uri -> [Diagnostic] -> LSP.FromServerMessage
publishDiagnosticsNotification uri diags =
    LSP.NotPublishDiagnostics $
    LSP.NotificationMessage "2.0" LSP.TextDocumentPublishDiagnostics $
    LSP.PublishDiagnosticsParams uri (List diags)

newtype Priority = Priority Double

setPriority :: Priority -> Action ()
setPriority (Priority p) = deprioritize p

sendEvent :: LSP.FromServerMessage -> Action ()
sendEvent e = do
    ShakeExtras{eventer} <- getShakeExtras
    liftIO $ eventer e

ideLogger :: IdeState -> Logger
ideLogger IdeState{shakeExtras=ShakeExtras{logger}} = logger

actionLogger :: Action Logger
actionLogger = do
    ShakeExtras{logger} <- getShakeExtras
    return logger


data GetModificationTime = GetModificationTime
    deriving (Eq, Show, Generic)
instance Hashable GetModificationTime
instance NFData   GetModificationTime
instance Binary   GetModificationTime

-- | Get the modification time of a file.
type instance RuleResult GetModificationTime = FileVersion

data FileVersion
    = VFSVersion Int
    | ModificationTime
      !Int   -- ^ Large unit (platform dependent, do not make assumptions)
      !Int   -- ^ Small unit (platform dependent, do not make assumptions)
    deriving (Show, Generic)

instance NFData FileVersion

vfsVersion :: FileVersion -> Maybe Int
vfsVersion (VFSVersion i) = Just i
vfsVersion ModificationTime{} = Nothing

-- | A comparision function where any VFS version is newer than an ondisk version
newerFileVersion :: FileVersion -> FileVersion -> Bool
newerFileVersion (VFSVersion i) (VFSVersion j) = i > j
newerFileVersion (VFSVersion {}) (ModificationTime {}) = True
newerFileVersion m1 m2 = modificationTime m1 > modificationTime m2

modificationTime :: FileVersion -> Maybe (Int, Int)
modificationTime VFSVersion{} = Nothing
modificationTime (ModificationTime large small) = Just (large, small)

getDiagnosticsFromStore :: StoreItem -> [Diagnostic]
getDiagnosticsFromStore (StoreItem _ diags) = concatMap SL.fromSortedList $ Map.elems diags


-- | Sets the diagnostics for a file and compilation step
--   if you want to clear the diagnostics call this with an empty list
setStageDiagnostics
    :: NormalizedFilePath
    -> TextDocumentVersion -- ^ the time that the file these diagnostics originate from was last edited
    -> T.Text
    -> [LSP.Diagnostic]
    -> DiagnosticStore
    -> DiagnosticStore
setStageDiagnostics fp timeM stage diags ds  =
    updateDiagnostics ds uri timeM diagsBySource
    where
        diagsBySource = Map.singleton (Just stage) (SL.toSortedList diags)
        uri = filePathToUri' fp

getAllDiagnostics ::
    DiagnosticStore ->
    [FileDiagnostic]
getAllDiagnostics =
    concatMap (\(k,v) -> map (fromUri k,ShowDiag,) $ getDiagnosticsFromStore v) . HMap.toList

getFileDiagnostics ::
    NormalizedFilePath ->
    DiagnosticStore ->
    [LSP.Diagnostic]
getFileDiagnostics fp ds =
    maybe [] getDiagnosticsFromStore $
    HMap.lookup (filePathToUri' fp) ds

filterDiagnostics ::
    (NormalizedFilePath -> Bool) ->
    DiagnosticStore ->
    DiagnosticStore
filterDiagnostics keep =
    HMap.filterWithKey (\uri _ -> maybe True (keep . toNormalizedFilePath') $ uriToFilePath' $ fromNormalizedUri uri)

filterVersionMap
    :: HMap.HashMap NormalizedUri (Set.Set TextDocumentVersion)
    -> HMap.HashMap NormalizedUri (Map TextDocumentVersion a)
    -> HMap.HashMap NormalizedUri (Map TextDocumentVersion a)
filterVersionMap =
    HMap.intersectionWith $ \versionsToKeep versionMap -> Map.restrictKeys versionMap versionsToKeep

updatePositionMapping :: IdeState -> VersionedTextDocumentIdentifier -> List TextDocumentContentChangeEvent -> IO ()
updatePositionMapping IdeState{shakeExtras = ShakeExtras{positionMapping}} VersionedTextDocumentIdentifier{..} changes = do
    modifyVar_ positionMapping $ \allMappings -> do
        let uri = toNormalizedUri _uri
        let mappingForUri = HMap.lookupDefault Map.empty uri allMappings
        let updatedMapping =
                Map.insert _version idMapping $
                Map.map (\oldMapping -> foldl' applyChange oldMapping changes) mappingForUri
        pure $! HMap.insert uri updatedMapping allMappings
