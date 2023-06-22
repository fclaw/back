{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Scaffold.Transport.Payload (Payload(..), valueToPayload) where

import Data.Aeson
import Data.Proxy
import Data.Swagger
import GHC.Generics
import Test.QuickCheck.Arbitrary
import Data.Aeson.KeyMap
import Data.Typeable (typeRep)
import Data.Text (pack)

-- | Swagger friendly wrapper over any JSON object
newtype Payload = Payload { getPayload :: Object }
  deriving stock Eq
  deriving stock Show
  deriving stock Generic
  deriving newtype ToJSON
  deriving newtype FromJSON

instance Arbitrary Payload where arbitrary = pure $ Payload empty

instance ToSchema Payload where
  declareNamedSchema _ = pure $ NamedSchema (Just (pack (show (typeRep (Proxy @Payload))))) $ toSchema (Proxy @Object)

valueToPayload :: Value -> Payload
valueToPayload (Object o) = Payload o
valueToPayload v = Payload $ singleton "value" v
