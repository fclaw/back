{-# LANGUAGE PackageImports #-}

module Hash (mkHash) where

import Control.Lens
import Control.Lens.Iso.Extended
import "hashing" Crypto.Hash
import Data.Text

mkHash :: Show a => a -> Text
mkHash x = show (hash (show x ^. stext . textbs) :: SHA256) ^. stext
