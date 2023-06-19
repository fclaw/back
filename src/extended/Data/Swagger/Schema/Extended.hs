{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Data.Swagger.Schema.Extended 
       ( schemaOptions
       , schemaOptionsDef
       , deriveToSchema
       , deriveToSchemaDef
       , deriveToSchemaFieldLabelModifier
       , module Data.Swagger.Schema
       ) where

import           Data.Aeson (defaultOptions)
import           Data.Aeson.Extended (aesonOptions) 
import           Data.Swagger.Schema
import           Language.Haskell.TH
import           Data.Time.Clock (DiffTime)
import           Data.Swagger
import           Data.Proxy (Proxy (..))
import           Control.Lens ((^.))
import           Type.Reflection (typeRep)
import           Control.Lens.Iso.Extended (stext)


schemaOptionsDef :: SchemaOptions
schemaOptionsDef = fromAesonOptions defaultOptions

schemaOptions :: String -> SchemaOptions
schemaOptions = fromAesonOptions . aesonOptions


deriveToSchema :: Name -> Q [Dec]
deriveToSchema name =
  [d|
    instance ToSchema $(conT name) where
      declareNamedSchema = 
       genericDeclareNamedSchema (schemaOptions $sname)
  |]
  where sname = return (LitE (StringL (nameBase name)))


deriveToSchemaDef :: Name -> Q [Dec]
deriveToSchemaDef name =
  [d|
    instance ToSchema $(conT name) where
      declareNamedSchema = 
       genericDeclareNamedSchema schemaOptionsDef
  |]

deriveToSchemaFieldLabelModifier :: Name -> Q Exp -> Q [Dec]
deriveToSchemaFieldLabelModifier name modify =
  [d|
    instance ToSchema $(conT name) where
      declareNamedSchema = 
       genericDeclareNamedSchema @($(conT name)) $ defaultSchemaOptions { fieldLabelModifier = $modify }
  |]


instance ToSchema DiffTime where
  declareNamedSchema _ = pure $ NamedSchema (Just $ (show (typeRep @DiffTime))^.stext) $ toSchema (Proxy @Int)
        