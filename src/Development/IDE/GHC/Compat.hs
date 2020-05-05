-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
#include "ghc-api-version.h"

-- | Attempt at hiding the GHC version differences we can.
module Development.IDE.GHC.Compat(
    getHeaderImports,
    HieFileResult(..),
    HieFile(..),
    NameCacheUpdater(..),
    RefMap,
    hieExportNames,
    mkHieFile,
    writeHieFile,
    readHieFile,
    supportsHieFiles,
    setDefaultHieDir,
    dontWriteHieFiles,
#if !MIN_GHC_API_VERSION(8,8,0)
    ml_hie_file,
#endif
    hPutStringBuffer,
    includePathsGlobal,
    includePathsQuote,
    addIncludePathsQuote,
    getModuleHash,
    getPackageName,
    pattern DerivD,
    pattern ForD,
    pattern InstD,
    pattern TyClD,
    pattern ValD,
    pattern ClassOpSig,
    pattern IEThingAll,
    pattern IEThingWith,
    GHC.ModLocation,
    Module.addBootSuffix,
    pattern ModLocation,

    upNameCache,

    module GHC,
#if MIN_GHC_API_VERSION(8,8,0)
    module HieTypes,
    module HieUtils,
#else
    module Development.IDE.GHC.HieTypes,
    module Development.IDE.GHC.HieUtils,
#endif
    ) where

import StringBuffer
import DynFlags
import FieldLabel
import Fingerprint (Fingerprint)
import qualified Module
import Packages
import Data.IORef
import HscTypes
import NameCache

import qualified GHC
import GHC hiding (ClassOpSig, DerivD, ForD, IEThingAll, IEThingWith, InstD, TyClD, ValD, ModLocation)
import qualified HeaderInfo as Hdr
import Avail
import ErrUtils (ErrorMessages)
import FastString (FastString)

import Development.IDE.GHC.HieAst (mkHieFile)
#if MIN_GHC_API_VERSION(8,6,0)
import Development.IDE.GHC.HieBin (readHieFile,writeHieFile,NameCacheUpdater(..),HieFileResult(..))
#endif

import Data.Map (Map)

#if MIN_GHC_API_VERSION(8,10,0)
import HscTypes (mi_mod_hash)
#endif

#if MIN_GHC_API_VERSION(8,8,0)
import HscTypes (srcErrorMessages)
import Control.Applicative ((<|>))
import Development.IDE.GHC.HieAst (mkHieFile)
import Development.IDE.GHC.HieBin
import HieUtils
import HieTypes
import IfaceEnv


import HieUtils

#else

#if MIN_GHC_API_VERSION(8,6,0)
import Development.IDE.GHC.HieTypes
import Development.IDE.GHC.HieUtils
#endif

import Control.Exception (catch)
import System.IO
import Foreign.ForeignPtr


hPutStringBuffer :: Handle -> StringBuffer -> IO ()
hPutStringBuffer hdl (StringBuffer buf len cur)
    = withForeignPtr (plusForeignPtr buf cur) $ \ptr ->
             hPutBuf hdl ptr len

#endif

upNameCache :: IORef NameCache -> (NameCache -> (NameCache, c)) -> IO c
#if !MIN_GHC_API_VERSION(8,8,0)
upNameCache ref upd_fn
  = atomicModifyIORef' ref upd_fn
#else
upNameCache = updNameCache
#endif
#if !MIN_GHC_API_VERSION(8,6,0)
includePathsGlobal, includePathsQuote :: [String] -> [String]
includePathsGlobal = id
includePathsQuote = const []
#endif


addIncludePathsQuote :: FilePath -> DynFlags -> DynFlags
#if MIN_GHC_API_VERSION(8,6,0)
addIncludePathsQuote path x = x{includePaths = f $ includePaths x}
    where f i = i{includePathsQuote = path : includePathsQuote i}
#else
addIncludePathsQuote path x = x{includePaths = path : includePaths x}
#endif

pattern DerivD :: DerivDecl p -> HsDecl p
pattern DerivD x <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.DerivD _ x
#else
    GHC.DerivD x
#endif

pattern ForD :: ForeignDecl p -> HsDecl p
pattern ForD x <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.ForD _ x
#else
    GHC.ForD x
#endif

pattern ValD :: HsBind p -> HsDecl p
pattern ValD x <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.ValD _ x
#else
    GHC.ValD x
#endif

pattern InstD :: InstDecl p -> HsDecl p
pattern InstD x <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.InstD _ x
#else
    GHC.InstD x
#endif

pattern TyClD :: TyClDecl p -> HsDecl p
pattern TyClD x <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.TyClD _ x
#else
    GHC.TyClD x
#endif

pattern ClassOpSig :: Bool -> [Located (IdP pass)] -> LHsSigType pass -> Sig pass
pattern ClassOpSig a b c <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.ClassOpSig _ a b c
#else
    GHC.ClassOpSig a b c
#endif

pattern IEThingWith :: LIEWrappedName (IdP pass) -> IEWildcard -> [LIEWrappedName (IdP pass)] -> [Located (FieldLbl (IdP pass))] -> IE pass
pattern IEThingWith a b c d <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.IEThingWith _ a b c d
#else
    GHC.IEThingWith a b c d
#endif

pattern ModLocation :: Maybe FilePath -> FilePath -> FilePath -> GHC.ModLocation
pattern ModLocation a b c <-
#if MIN_GHC_API_VERSION(8,8,0)
    GHC.ModLocation a b c _ where ModLocation a b c = GHC.ModLocation a b c ""
#else
    GHC.ModLocation a b c where ModLocation a b c = GHC.ModLocation a b c
#endif

pattern IEThingAll :: LIEWrappedName (IdP pass) -> IE pass
pattern IEThingAll a <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.IEThingAll _ a
#else
    GHC.IEThingAll a
#endif

setDefaultHieDir :: FilePath -> DynFlags -> DynFlags
setDefaultHieDir _f d =
#if MIN_GHC_API_VERSION(8,8,0)
    d { hieDir     = hieDir d <|> Just _f}
#else
    d
#endif

dontWriteHieFiles :: DynFlags -> DynFlags
dontWriteHieFiles d =
#if MIN_GHC_API_VERSION(8,8,0)
    gopt_unset d Opt_WriteHie
#else
    d
#endif

nameListFromAvails :: [AvailInfo] -> [(SrcSpan, Name)]
nameListFromAvails as =
  map (\n -> (nameSrcSpan n, n)) (concatMap availNames as)

#if !MIN_GHC_API_VERSION(8,8,0)
-- Reimplementations of functions for HIE files for GHC 8.6

ml_hie_file :: GHC.ModLocation -> FilePath
ml_hie_file ml = ml_hi_file ml ++ ".hie"
#endif

getHeaderImports
  :: DynFlags
  -> StringBuffer
  -> FilePath
  -> FilePath
  -> IO
       ( Either
           ErrorMessages
           ( [(Maybe FastString, Located ModuleName)]
           , [(Maybe FastString, Located ModuleName)]
           , Located ModuleName
           )
       )
#if MIN_GHC_API_VERSION(8,8,0)
getHeaderImports = Hdr.getImports
#else
getHeaderImports a b c d =
    catch (Right <$> Hdr.getImports a b c d)
          (return . Left . srcErrorMessages)
#endif
getModuleHash :: ModIface -> Fingerprint
#if MIN_GHC_API_VERSION(8,10,0)
getModuleHash = mi_mod_hash . mi_final_exts
#else
getModuleHash = mi_mod_hash
#endif

type RefMap = Map Identifier [(Span, IdentifierDetails TypeIndex)]

supportsHieFiles :: Bool
supportsHieFiles = True

hieExportNames :: HieFile -> [(SrcSpan, Name)]
hieExportNames = nameListFromAvails . hie_exports

getPackageName :: DynFlags -> Module.InstalledUnitId -> Maybe PackageName
getPackageName dfs i = packageName <$> lookupPackage dfs (Module.DefiniteUnitId (Module.DefUnitId i))

