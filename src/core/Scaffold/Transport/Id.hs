{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Scaffold.Transport.Id (Id (..)) where

import Data.Aeson
import Data.Binary
import Data.Default.Class
import Data.Hashable
import Data.Int
import Data.Swagger
import Database.Transaction (ParamsShow (..))
import GHC.Generics
import GHC.Types
import Servant.API
import Test.QuickCheck (Arbitrary)
import TextShow

-- | Type for ids that is shared across all the projects.
-- user id is int64, so it's encrypted as "text" in json,
-- otherwise js code may fail to work with it.
newtype Id (a :: Symbol) = Id Int64
  deriving newtype (Show, Eq)
  deriving newtype (Arbitrary)
  deriving newtype (Binary)
  deriving newtype (Hashable)
  deriving newtype (Read)
  deriving newtype (Num)
  deriving newtype (Enum)
  deriving newtype (Real)
  deriving newtype (Integral)
  deriving newtype (TextShow)
  deriving stock (Generic)
  deriving stock (Ord)
  deriving anyclass (ToParamSchema)
  deriving newtype (FromHttpApiData)
  deriving newtype (ParamsShow)
  deriving newtype (ToJSON)
  deriving newtype (FromJSON)

instance ToSchema (Id a)

instance Default (Id a)
