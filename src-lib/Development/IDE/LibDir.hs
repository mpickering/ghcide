module Development.IDE.LibDir where
-- This module exists purely to keep the TH stage restriction happy

import qualified GHC.Paths
import System.Environment
import Data.Maybe

-- Set the GHC libdir to the nix libdir if it's present.
getLibdir :: IO FilePath
getLibdir = fromMaybe GHC.Paths.libdir <$> lookupEnv "NIX_GHC_LIBDIR"
