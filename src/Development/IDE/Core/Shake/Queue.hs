-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ExplicitNamespaces         #-}

-- | The ShakeQueue which mediates communication between the server and
-- shake database.
module Development.IDE.Core.Shake.Queue (DelayedAction, DelayedActionInternal, pattern DelayedAction, pattern DelayedActionInternal, mkDelayedAction, getAction, actionName, actionPriority) where

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

-- This is stuff we make up and add onto the information the user
-- provided.
data DelayedActionExtra = DelayedActionExtra { actionInternalId :: Int
                                             , actionInternalFinished :: IO Bool
                                             , actionInternal :: Action ()
                                             }

type DelayedAction a = DelayedActionX (Action a)
type DelayedActionInternal = DelayedActionX DelayedActionExtra

{-# COMPLETE DelayedActionInternal#-}
pattern DelayedActionInternal :: String -> Logger.Priority -> Action () -> Int -> IO Bool -> DelayedActionX DelayedActionExtra
pattern DelayedActionInternal {actionInternalName, actionInternalPriority, getAction
                              , actionId, actionFinished}
                              = DelayedActionX actionInternalName  actionInternalPriority
                                    (DelayedActionExtra actionId actionFinished getAction)

{-# COMPLETE DelayedAction#-}
pattern DelayedAction :: String ->  Logger.Priority -> Action a -> DelayedAction a
pattern DelayedAction a b c = DelayedActionX a b c

mkDelayedAction :: String -> Logger.Priority -> Action a -> DelayedAction a
mkDelayedAction = DelayedAction

data DelayedActionX a = DelayedActionX { actionName :: String -- Name we show to the user
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

