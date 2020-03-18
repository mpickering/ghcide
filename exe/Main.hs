-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-dodgy-imports #-} -- GHC no longer exports def in GHC 8.6 and above
{-# LANGUAGE CPP #-} -- To get precise GHC version
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main(main) where

import Linker (initDynLinker)
import Data.IORef
import NameCache
import Packages
import Module
import Arguments
import Data.Maybe
import Data.List.Extra
import System.FilePath
import Control.Concurrent.Extra
import Control.Exception
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Default
import System.Time.Extra
import Development.IDE.Core.Debouncer
import Development.IDE.Core.FileStore
import Development.IDE.Core.OfInterest
import Development.IDE.Core.Service
import Development.IDE.Core.Rules
import Development.IDE.Core.Shake
import Development.IDE.Core.RuleTypes
import Development.IDE.LSP.Protocol
import Development.IDE.Types.Location
import Development.IDE.Types.Diagnostics
import Development.IDE.Types.Options
import Development.IDE.Types.Logger
import Development.IDE.GHC.Util
import Development.IDE.Plugin
import Development.IDE.Plugin.Completions as Completions
import Development.IDE.Plugin.CodeAction as CodeAction
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types (LspId(IdInt))
import Data.Version
import Development.IDE.LSP.LanguageServer
import qualified System.Directory.Extra as IO
import System.Environment
import System.IO
import System.Exit
import HIE.Bios.Environment (addCmdOpts)
import Paths_ghcide
import Development.GitRev
import Development.Shake (Action,  action)
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import Data.Either
--import Outputable (pprTraceM, ppr, text)
import qualified Crypto.Hash.SHA1               as H
import qualified Data.ByteString.Char8          as B
import           Data.ByteString.Base16         (encode)

import           DynFlags                       (gopt_set, gopt_unset,
                                                 updOptLevel)

import GhcMonad
import HscTypes (HscEnv(..), ic_dflags)
import DynFlags (PackageFlag(..), PackageArg(..))
import GHC hiding (def)
import qualified GHC.Paths

import           HIE.Bios.Cradle
import           HIE.Bios.Types
import System.Directory

--import Rules
--import RuleTypes
--
-- Set the GHC libdir to the nix libdir if it's present.
getLibdir :: IO FilePath
getLibdir = fromMaybe GHC.Paths.libdir <$> lookupEnv "NIX_GHC_LIBDIR"

ghcideVersion :: IO String
ghcideVersion = do
  path <- getExecutablePath
  let gitHashSection = case $(gitHash) of
        x | x == "UNKNOWN" -> ""
        x -> " (GIT hash: " <> x <> ")"
  return $ "ghcide version: " <> showVersion version
             <> " (GHC: " <> VERSION_ghc
             <> ") (PATH: " <> path <> ")"
             <> gitHashSection

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

    let plugins = Completions.plugin <> CodeAction.plugin
        onInitialConfiguration = const $ Right ()
        onConfigurationChange  = const $ Right ()

    if argLSP then do
        t <- offsetTime
        hPutStrLn stderr "Starting LSP server..."
        hPutStrLn stderr "If you are seeing this in a terminal, you probably should have run ghcide WITHOUT the --lsp option!"
        runLanguageServer def (pluginHandler plugins) onInitialConfiguration onConfigurationChange $ \getLspId event vfs caps -> do
            t <- t
            hPutStrLn stderr $ "Started LSP server in " ++ showDuration t
            let options = (defaultIdeOptions $ loadSession dir)
                    { optReportProgress = clientSupportsProgress caps
                    , optShakeProfiling = argsShakeProfiling
                    , optTesting        = argsTesting
                    , optInterfaceLoadingDiagnostics = argsTesting
                    }
            debouncer <- newAsyncDebouncer
            initialise caps (mainRule >> pluginRules plugins >> action kick)
                getLspId event (logger minBound) debouncer options vfs
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
        ide <- initialise def mainRule (pure $ IdInt 0) (showEvent lock) (logger minBound) debouncer (defaultIdeOptions $ loadSession dir) vfs

        putStrLn "\nStep 4/6: Type checking the files"
        setFilesOfInterest ide $ HashSet.fromList $ map toNormalizedFilePath files
        _ <- runActionSync ide $ uses TypeCheck (map toNormalizedFilePath files)
--        results <- runActionSync ide $ use TypeCheck $ toNormalizedFilePath "src/Development/IDE/Core/Rules.hs"
--        results <- runActionSync ide $ use TypeCheck $ toNormalizedFilePath "exe/Main.hs"
        return ()

expandFiles :: [FilePath] -> IO [FilePath]
expandFiles = concatMapM $ \x -> do
    b <- IO.doesFileExist x
    if b then return [x] else do
        let recurse "." = True
            recurse x | "." `isPrefixOf` takeFileName x = False -- skip .git etc
            recurse x = takeFileName x `notElem` ["dist","dist-newstyle"] -- cabal directories
        files <- filter (\x -> takeExtension x `elem` [".hs",".lhs"]) <$> IO.listFilesInside (return . recurse) x
        when (null files) $
            fail $ "Couldn't find any .hs/.lhs files inside directory: " ++ x
        return files


kick :: Action ()
kick = do
    files <- getFilesOfInterest
    void $ uses TypeCheck $ HashSet.toList files

-- | Print an LSP event.
showEvent :: Lock -> FromServerMessage -> IO ()
showEvent _ (EventFileDiagnostics _ []) = return ()
showEvent lock (EventFileDiagnostics (toNormalizedFilePath -> file) diags) =
    withLock lock $ T.putStrLn $ showDiagnosticsColored $ map (file,ShowDiag,) diags
showEvent lock e = withLock lock $ print e


cradleToSessionOpts :: Cradle a -> FilePath -> IO ComponentOptions
cradleToSessionOpts cradle file = do
    let showLine s = putStrLn ("> " ++ s)
    cradleRes <- runCradle (cradleOptsProg cradle) showLine file
    opts <- case cradleRes of
        CradleSuccess r -> pure r
        CradleFail err -> throwIO err
        -- TODO Rather than failing here, we should ignore any files that use this cradle.
        -- That will require some more changes.
        CradleNone -> fail "'none' cradle is not yet supported"
    pure opts

emptyHscEnv :: IO HscEnv
emptyHscEnv = do
    libdir <- getLibdir
    env <- runGhc (Just libdir) getSession
    initDynLinker env
    pure env

-- Convert a target to a list of potential absolute paths.
-- A TargetModule can be anywhere listed by the supplied include
-- directories
-- A target file is a relative path but with a specific prefix so just need
-- to canonicalise it.
targetToFile :: [FilePath] -> TargetId -> IO [NormalizedFilePath]
targetToFile is (TargetModule mod) = do
    let fps = [i </> (moduleNameSlashes mod) -<.> ext | ext <- exts, i <- is ]
        exts = ["hs", "hs-boot", "lhs"]
    mapM (fmap (toNormalizedFilePath) . canonicalizePath) fps
targetToFile _ (TargetFile f _) = do
  f' <- canonicalizePath f
  return [(toNormalizedFilePath f')]

setNameCache :: IORef NameCache -> HscEnv -> HscEnv
setNameCache nc hsc = hsc { hsc_NC = nc }

-- This is the key function which implements multi-component support. All
-- components mapping to the same hie,yaml file are mapped to the same
-- HscEnv which is updated as new components are discovered.
loadSession :: FilePath -> Action (FilePath -> Action HscEnvEq)
loadSession dir = liftIO $ do
    -- Mapping from hie.yaml file to HscEnv, one per hie.yaml file
    hscEnvs <- newVar Map.empty
    -- Mapping from a filepath to HscEnv
    fileToFlags <- newVar Map.empty
    -- This caches the mapping from Mod.hs -> hie.yaml
    cradleLoc <- memoIO $ \v -> do
        res <- findCradle v
        -- Sometimes we get C:, sometimes we get c:, and sometimes we get a relative path
        -- try and normalise that
        -- e.g. see https://github.com/digital-asset/ghcide/issues/126
        res' <- traverse IO.makeAbsolute res
        return $ normalise <$> res'

    -- Create a new HscEnv from a hieYaml root and a set of options
    -- If the hieYaml file already has an HscEnv, the new component is
    -- combined with the components in the old HscEnv into a new HscEnv
    -- which contains both.
    packageSetup <- return $ \(hieYaml, opts) -> do
        -- Parse DynFlags for the newly discovered component
        hscEnv <- emptyHscEnv
        (df, targets) <- evalGhcEnv hscEnv $ do
                          setOptions opts (hsc_dflags hscEnv)
        -- Now lookup to see whether we are combining with an exisiting HscEnv
        -- or making a new one. The lookup returns the HscEnv and a list of
        -- information about other components loaded into the HscEnv
        -- (unitId, DynFlag, Targets)
        modifyVar hscEnvs $ \m -> do
            -- Just deps if there's already an HscEnv
            -- Nothing is it's the first time we are making an HscEnv
            let oldDeps = Map.lookup hieYaml m
            let -- Add the raw information about this component to the list
                -- We will modify the unitId and DynFlags used for
                -- compilation but these are the true source of
                -- information.
                new_deps = (thisInstalledUnitId df, df, targets) : maybe [] snd oldDeps
                -- Get all the unit-ids for things in this component
                inplace = map (\(a, _, _) -> a) new_deps
                -- Remove all inplace dependencies from package flags for
                -- components in this HscEnv
                rearrange (uid, (df, uids), ts) = (uid, (df, uids, ts))
                do_one (uid,df, ts) = rearrange (uid, removeInplacePackages inplace df, ts)
                -- All deps, but without any packages which are also loaded
                -- into memory
                new_deps' = map do_one new_deps
            -- Make a new HscEnv, we have to recompile everything from
            -- scratch again (for now)
            -- It's important to keep the same NameCache though for reasons
            -- that I do not fully understand
            print ("Making new HscEnv" ++ (show inplace))
            hscEnv <- case oldDeps of
                        Nothing -> emptyHscEnv
                        Just (old_hsc, _) -> setNameCache (hsc_NC old_hsc) <$> emptyHscEnv
            newHscEnv <-
              -- Add the options for the current component to the HscEnv
              evalGhcEnv hscEnv $ do
                _ <- setSessionDynFlags df
                getSession
            -- Modify the map so the hieYaml now maps to the newly created
            -- HscEnv
            -- Returns
            -- * the new HscEnv so it can be used to modify the
            --   FilePath -> HscEnv map
            -- * The information for the new component which caused this cache miss
            -- * The modified information (without -inplace flags) for
            --   existing packages
            pure (Map.insert hieYaml (newHscEnv, new_deps) m, (newHscEnv, head new_deps', tail new_deps'))


    session <- return $ \(hieYaml, opts) -> do
        (hscEnv, new, old_deps) <- packageSetup (hieYaml, opts)
        -- TODO Handle the case where there is no hie.yaml
        -- Make a map from unit-id to DynFlags, this is used when trying to
        -- resolve imports.
        let uids = map (\(iuid, (df, _uis, _targets)) -> (iuid, df)) (new : old_deps)

        -- For each component, now make a new HscEnvEq which contains the
        -- HscEnv for the hie.yaml file but the DynFlags for that component
        --
        -- Then look at the targets for each component and create a map
        -- from FilePath to the HscEnv
        let new_cache (_iuid, (df, _uis, targets)) =  do
              let hscEnv' = hscEnv { hsc_dflags = df
                                   , hsc_IC = (hsc_IC hscEnv) { ic_dflags = df } }

              res <- newHscEnvEq hscEnv' uids

              let is = importPaths df
              ctargets <- concatMapM (targetToFile is  . targetId) targets
              --pprTraceM "TARGETS" (ppr (map (text . show) ctargets))
              let xs = map (,res) ctargets
              return (xs, res)

        -- New HscEnv for the component in question
        (cs, res) <- new_cache new
        -- Modified cache targets for everything else in the hie.yaml file
        -- which now uses the same EPS and so on
        cached_targets <- concatMapM (fmap fst . new_cache) old_deps
        modifyVar_ fileToFlags $ \var -> do
            pure $ Map.insert hieYaml (cs ++ cached_targets) var
        return res

    lock <- newLock

    -- This caches the mapping from hie.yaml + Mod.hs -> [String]
    sessionOpts <- return $ \(hieYaml, file) -> do
        fm <- readVar fileToFlags
        let mv = Map.lookup hieYaml fm
        let v = fromMaybe [] mv
        cfp <- liftIO $ canonicalizePath file
        -- We sort so exact matches come first.
        case find (\(f', _) -> fromNormalizedFilePath f' == cfp) v of
            Just (_, opts) -> do
                putStrLn $ "Cached component of " <> show file
                pure opts
            Nothing-> do
                putStrLn $ "Shelling out to cabal " <> show file
                cradle <- maybe (loadImplicitCradle $ addTrailingPathSeparator dir) loadCradle hieYaml
                opts <- cradleToSessionOpts cradle file
                print opts
                session (hieYaml, opts)
    return $ \file -> liftIO $ withLock lock $ do
        hieYaml <- cradleLoc file
        sessionOpts (hieYaml, file)

-- This function removes all the -package flags which refer to packages we
-- are going to deal with ourselves. For example, if a executable depends
-- on a library component, then this function will remove the library flag
-- from the package flags for the executable
--
-- There are several places in GHC (for example the call to hptInstances in
-- tcRnImports) which assume that all modules in the HPT have the same unit
-- ID. Therefore we create a fake one and give them all the same unit id.
removeInplacePackages :: [InstalledUnitId] -> DynFlags -> (DynFlags, [InstalledUnitId])
removeInplacePackages us df = (df { packageFlags = ps
                                  , thisInstalledUnitId = fake_uid }, uids)
  where
    (uids, ps) = partitionEithers (map go (packageFlags df))
    fake_uid = toInstalledUnitId (stringToUnitId "fake_uid")
    go p@(ExposePackage _ (UnitIdArg u) _) = if (toInstalledUnitId u `elem` us) then Left (toInstalledUnitId u) else Right p
    go p = Right p

-- | Memoize an IO function, with the characteristics:
--
--   * If multiple people ask for a result simultaneously, make sure you only compute it once.
--
--   * If there are exceptions, repeatedly reraise them.
--
--   * If the caller is aborted (async exception) finish computing it anyway.
memoIO :: Ord a => (a -> IO b) -> IO (a -> IO b)
memoIO op = do
    ref <- newVar Map.empty
    return $ \k -> join $ mask_ $ modifyVar ref $ \mp ->
        case Map.lookup k mp of
            Nothing -> do
                res <- onceFork $ op k
                return (Map.insert k res mp, res)
            Just res -> return (mp, res)

setOptions :: GhcMonad m => ComponentOptions -> DynFlags -> m (DynFlags, [Target])
setOptions (ComponentOptions theOpts _) dflags = do
    cacheDir <- liftIO $ getCacheDir theOpts
    (dflags', targets) <- addCmdOpts theOpts dflags
    let dflags'' =
          -- disabled, generated directly by ghcide instead
          flip gopt_unset Opt_WriteInterface $
          -- disabled, generated directly by ghcide instead
          -- also, it can confuse the interface stale check
          dontWriteHieFiles $
          setHiDir cacheDir $
          setDefaultHieDir cacheDir $
          setIgnoreInterfacePragmas $
          setLinkerOptions $
          disableOptimisation dflags'
    -- initPackages parses the -package flags and
    -- sets up the visibility for each component.
    (final_df, _) <- liftIO $ initPackages dflags''
--    let df'' = gopt_unset df' Opt_WarnIsError
    return (final_df, targets)


-- we don't want to generate object code so we compile to bytecode
-- (HscInterpreted) which implies LinkInMemory
-- HscInterpreted
setLinkerOptions :: DynFlags -> DynFlags
setLinkerOptions df = df {
    ghcLink   = LinkInMemory
  , hscTarget = HscNothing
  , ghcMode = CompManager
  }

setIgnoreInterfacePragmas :: DynFlags -> DynFlags
setIgnoreInterfacePragmas df =
    gopt_set (gopt_set df Opt_IgnoreInterfacePragmas) Opt_IgnoreOptimChanges

disableOptimisation :: DynFlags -> DynFlags
disableOptimisation df = updOptLevel 0 df

setHiDir :: FilePath -> DynFlags -> DynFlags
setHiDir f d =
    -- override user settings to avoid conflicts leading to recompilation
    d { hiDir      = Just f}

getCacheDir :: [String] -> IO FilePath
getCacheDir opts = IO.getXdgDirectory IO.XdgCache (cacheDir </> opts_hash)
    where
        -- Create a unique folder per set of different GHC options, assuming that each different set of
        -- GHC options will create incompatible interface files.
        opts_hash = B.unpack $ encode $ H.finalize $ H.updates H.init (map B.pack opts)

-- Prefix for the cache path
cacheDir :: String
cacheDir = "ghcide"

