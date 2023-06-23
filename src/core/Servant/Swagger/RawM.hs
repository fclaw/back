{-# LANGUAGE FlexibleInstances #-}

module Servant.Swagger.RawM () where

import Control.Lens
import Data.Swagger.Lens
import Servant.RawM
import Servant.Swagger

instance HasSwagger RawM where toSwagger _ = mempty & paths . at "/" ?~ mempty
