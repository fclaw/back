{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Scaffold.Transport.Error
  ( Error (..),
    AsError (..),
    Located (..),
    addMeta,
    appendMetas,
    appendMeta,
  )
where

import Control.Exception
import Control.Lens hiding ((.=))
import Data.Aeson.Extended hiding (Error)
import Data.Bifunctor
-- import Data.Aeson.KeyMap
import Data.Proxy
import Data.Swagger
import qualified Data.Text as T
-- import qualified Data.Vector as V
import GHC.Generics
import Scaffold.Transport.Payload

-- | Generic error. Keeps information about the problem and
-- additional metadata.
data Error = Error
  { -- | Human readable message.
    errorMessage :: T.Text,
    -- | Message metadata.
    errorMeta :: Maybe Payload
  }
  deriving stock (Show)
  deriving stock (Generic)

instance Exception Error

-- | How to convert concrete error into generic one.
class AsError e where
  asError :: e -> Error

instance AsError Error where asError = id

-- | Add metadata to the existing error.
addMeta :: ToJSON a => T.Text -> a -> Error -> Error
addMeta name t Error {..} = undefined

-- | Append metadata to the list of arrays.
appendMeta :: ToJSON a => T.Text -> a -> Error -> Error
appendMeta name t Error {..} = undefined

appendMetas :: ToJSON a => T.Text -> [a] -> Error -> Error
appendMetas name ts Error {..} = undefined

instance ToJSON Error where
  toJSON Error {..} =
    object
      ( "message"
          .= errorMessage
          : case errorMeta of
            Nothing -> mempty
            Just x -> ["meta" .= x]
      )

instance FromJSON Error where
  parseJSON = withObject "error" $ \o -> do
    errorMessage <- o .: "message"
    errorMeta <- o .: "meta"
    pure Error {..}

instance AsError T.Text where
  asError t = Error t Nothing

instance ToSchema Error where
  declareNamedSchema _ = do
    tSchema <- declareSchemaRef (Proxy @T.Text)
    oSchema <- declareSchemaRef (Proxy @Object)
    pure $
      NamedSchema (Just $ "Error") $
        mempty
          & type_ ?~ SwaggerObject
          & properties .~ [("message", tSchema), ("meta", oSchema)]
          & required .~ ["message"]

-- | Error with corresponding locations.
data Located at err = Located
  { locations :: [at],
    value :: err
  }
  deriving stock (Functor)

instance (ToJSON l, AsError v) => AsError (Located l v) where
  asError (Located l v) = appendMetas "source" l $ asError v

instance Bifunctor Located where
  first f (Located a b) = Located (f <$> a) b
  second g (Located a b) = Located a (g b)
