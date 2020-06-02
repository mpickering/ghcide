{-# LANGUAGE ExistentialQuantification  #-}
module Development.IDE.Core.Shake.Key where

import Data.Hashable
import Data.Typeable

-- | Key type
data Key = forall k . (Typeable k, Hashable k, Eq k, Show k) => Key k

instance Show Key where
  show (Key k) = show k

instance Eq Key where
    Key k1 == Key k2 | Just k2' <- cast k2 = k1 == k2'
                     | otherwise = False

instance Hashable Key where
    hashWithSalt salt (Key key) = hashWithSalt salt (typeOf key, key)
