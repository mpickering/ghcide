-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-dodgy-imports #-} -- GHC no longer exports def in GHC 8.6 and above
{-# LANGUAGE CPP #-} -- To get precise GHC version
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main(main) where

import Arguments
import Control.Concurrent.Async
import Control.Concurrent.Extra
import Control.Monad.Extra
import Data.Default
import qualified Data.HashSet as HashSet
import Data.List.Extra
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Development.IDE.BiosRules
import Development.IDE.Core.Debouncer
import Development.IDE.Core.FileStore
import Development.IDE.Core.OfInterest
import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Rules
import Development.IDE.Core.Service
import Development.IDE.Core.Shake
import Development.IDE.LSP.LanguageServer
import Development.IDE.Plugin
import Development.IDE.Plugin.CodeAction as CodeAction
import Development.IDE.Plugin.Completions as Completions
import Development.IDE.Types.Location
import Development.IDE.Types.Logger
import Development.IDE.Types.Options
import HIE.Bios.Cradle
import qualified Language.Haskell.LSP.Core as LSP
import Language.Haskell.LSP.Types (LspId(IdInt))
import qualified System.Directory.Extra as IO
import System.Exit
import System.IO
import System.Time.Extra


-- import Debug.Trace


main :: IO ()
main = do
    -- WARNING: If you write to stdout before runLanguageServer
    --          then the language server will not work
    Arguments{..} <- getArguments

    if argsVersion then ghcideVersion >>= putStrLn >> exitSuccess
    else hPutStrLn stderr {- see WARNING above -} =<< ghcideVersion

    -- lock to avoid overlapping output on stdout
    lock <- newLock
    let logger p = Logger $ \pri msg -> when (pri >= p) $ withLock lock $
            T.putStrLn $ T.pack ("[" ++ upper (show pri) ++ "] ") <> msg

    whenJust argsCwd IO.setCurrentDirectory

    dir <- IO.getCurrentDirectory
    command <- makeLspCommandId "typesignature.add"

    let plugins = Completions.plugin <> CodeAction.plugin
        onInitialConfiguration = const $ Right ()
        onConfigurationChange  = const $ Right ()
        options = def { LSP.executeCommandCommands = Just [command]
                      , LSP.completionTriggerCharacters = Just "."
                      }

    if argLSP then do
        t <- offsetTime
        hPutStrLn stderr "Starting LSP server..."
        hPutStrLn stderr "If you are seeing this in a terminal, you probably should have run ghcide WITHOUT the --lsp option!"
        runLanguageServer options (pluginHandler plugins) onInitialConfiguration onConfigurationChange $ \getLspId event vfs caps -> do
            t <- t
            hPutStrLn stderr $ "Started LSP server in " ++ showDuration t
            let options = (defaultIdeOptions $ loadSession dir)
                    { optReportProgress = clientSupportsProgress caps
                    , optShakeProfiling = argsShakeProfiling
                    , optTesting        = argsTesting
                    , optThreads        = argsThreads
                    , optInterfaceLoadingDiagnostics = argsTesting
                    }
                logLevel = if argsVerbose then minBound else Info
            debouncer <- newAsyncDebouncer
            fst <$> initialise caps (mainRule >> pluginRules plugins)
                      getLspId event (logger logLevel) debouncer options vfs
    else do
        -- GHC produces messages with UTF8 in them, so make sure the terminal doesn't error
        hSetEncoding stdout utf8
        hSetEncoding stderr utf8

        putStrLn $ "Ghcide setup tester in " ++ dir ++ "."
        putStrLn "Report bugs at https://github.com/digital-asset/ghcide/issues"

        putStrLn $ "\nStep 1/6: Finding files to test in " ++ dir
        files <- expandFiles (argFiles ++ ["." | null argFiles])
        -- LSP works with absolute file paths, so try and behave similarly
        files <- nubOrd <$> mapM IO.canonicalizePath files
        putStrLn $ "Found " ++ show (length files) ++ " files"

        putStrLn "\nStep 2/6: Looking for hie.yaml files that control setup"
        cradles <- mapM findCradle files
        let ucradles = nubOrd cradles
        let n = length ucradles
        putStrLn $ "Found " ++ show n ++ " cradle" ++ ['s' | n /= 1]
        putStrLn "\nStep 3/6: Initializing the IDE"
        vfs <- makeVFSHandle
        debouncer <- newAsyncDebouncer
        (ide, worker) <- initialise def mainRule (pure $ IdInt 0) (showEvent lock) (logger Debug) debouncer (defaultIdeOptions $ loadSession dir) vfs

        putStrLn "\nStep 4/6: Type checking the files"
        setFilesOfInterest ide $ HashSet.fromList $ map toNormalizedFilePath' files
        _ <- runActionSync "TypecheckTest" ide $ uses TypeCheck (map toNormalizedFilePath' files)
--        results <- runActionSync ide $ use TypeCheck $ toNormalizedFilePath' "src/Development/IDE/Core/Rules.hs"
        {-
        let fp =  toNormalizedFilePath' "ghc/Main.hs"
        results <- runActionSync "tc" ide $ use TypeCheck $ toNormalizedFilePath' "ghc/Main.hs"
        hover1 <- duration $ runIdeAction "Hover" ide $ getAtPoint fp (Position 950 20)
        print hover1
        traceMarkerIO "START"
        hover2 <- duration $ runIdeAction "Hover" ide $ getAtPoint fp (Position 950 20)
        print hover2
        traceMarkerIO "END"
        -}
        cancel worker
        return ()
