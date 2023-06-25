{-# LANGUAGE TemplateHaskell #-}

module Data.Aeson.TH.Extended (underscoreOptions, deriveFromJSONKey, deriveToJSONKey, module Data.Aeson.TH) where

import Data.Aeson (FromJSONKey (..), ToJSONKey (..), defaultJSONKeyOptions, genericFromJSONKey, genericToJSONKey, keyModifier)
import Data.Aeson.TH
import Data.Char (toLower)
import Language.Haskell.TH

underscoreOptions :: Options
underscoreOptions = defaultOptions {fieldLabelModifier = tail, constructorTagModifier = map toLower}

deriveFromJSONKey :: Name -> Q Exp -> Q [Dec]
deriveFromJSONKey name modify =
  [d|
    instance FromJSONKey $(conT name) where
      fromJSONKey = genericFromJSONKey defaultJSONKeyOptions {keyModifier = $modify}
    |]

deriveToJSONKey :: Name -> Q Exp -> Q [Dec]
deriveToJSONKey name modify =
  [d|
    instance ToJSONKey $(conT name) where
      toJSONKey = genericToJSONKey defaultJSONKeyOptions {keyModifier = $modify}
    |]
