{-# LANGUAGE ConstraintKinds #-}
{-
data MyConfig = MyConfig { mcNameOfProcess :: String
                         , mcArgsToProcess :: [String]
                         }
  deriving (Read, Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier     '[CamelTo2 "_" , Drop 2]
                        , ConstructorTagModifier '[CamelTo2 "_"]
                        ]
                        MyConfig

data Init
instance Reifies Init (String -> String) where
  reflect _ = init

data OtherConfig = OtherConfig { otrNameOfProcess :: Maybe String
                               , otrArgsToProcess :: [String]
                               }
  deriving (Read, Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier     '[CamelTo2 "-"]
                        , ConstructorTagModifier '[CamelTo2 "-", UserDefined Init]
                        , SumEnc                  TwoElemArr
                        , TagSingleConstructors  'True
                        , OmitNothingFields      'True
                        ]
                        OtherConfig
-}
{-
data MyConfig = MyConfig { mcNameOfProcess :: String
                         , mcArgsToProcess :: [String]
                         }
  deriving (Read, Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier     '[CamelTo2 "_" , Drop 2]
                        , ConstructorTagModifier '[CamelTo2 "_"]
                        ]
                        MyConfig

data Init
instance Reifies Init (String -> String) where
  reflect _ = init

data OtherConfig = OtherConfig { otrNameOfProcess :: Maybe String
                               , otrArgsToProcess :: [String]
                               }
  deriving (Read, Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier     '[CamelTo2 "-"]
                        , ConstructorTagModifier '[CamelTo2 "-", UserDefined Init]
                        , SumEnc                  TwoElemArr
                        , TagSingleConstructors  'True
                        , OmitNothingFields      'True
                        ]
                        OtherConfig
-}
{-
data MyConfig = MyConfig { mcNameOfProcess :: String
                         , mcArgsToProcess :: [String]
                         }
  deriving (Read, Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier     '[CamelTo2 "_" , Drop 2]
                        , ConstructorTagModifier '[CamelTo2 "_"]
                        ]
                        MyConfig

data Init
instance Reifies Init (String -> String) where
  reflect _ = init

data OtherConfig = OtherConfig { otrNameOfProcess :: Maybe String
                               , otrArgsToProcess :: [String]
                               }
  deriving (Read, Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier     '[CamelTo2 "-"]
                        , ConstructorTagModifier '[CamelTo2 "-", UserDefined Init]
                        , SumEnc                  TwoElemArr
                        , TagSingleConstructors  'True
                        , OmitNothingFields      'True
                        ]
                        OtherConfig
-}
{-
data MyConfig = MyConfig { mcNameOfProcess :: String
                         , mcArgsToProcess :: [String]
                         }
  deriving (Read, Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier     '[CamelTo2 "_" , Drop 2]
                        , ConstructorTagModifier '[CamelTo2 "_"]
                        ]
                        MyConfig

data Init
instance Reifies Init (String -> String) where
  reflect _ = init

data OtherConfig = OtherConfig { otrNameOfProcess :: Maybe String
                               , otrArgsToProcess :: [String]
                               }
  deriving (Read, Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier     '[CamelTo2 "-"]
                        , ConstructorTagModifier '[CamelTo2 "-", UserDefined Init]
                        , SumEnc                  TwoElemArr
                        , TagSingleConstructors  'True
                        , OmitNothingFields      'True
                        ]
                        OtherConfig
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Generic.DerivingVia
  ( StrFun (..),
    Setting (..),
    SumEncoding' (..),
    DefaultOptions,
    WithOptions (..),
    -- Utility type synonyms to save ticks (') before promoted data constructors
    type Drop,
    type CamelTo2,
    type UserDefined,
    type TaggedObj,
    type UntaggedVal,
    type ObjWithSingleField,
    type TwoElemArr,
    type FieldLabelModifier,
    type ConstructorTagModifier,
    type AllNullaryToStringTag,
    type OmitNothingFields,
    type SumEnc,
    type UnwrapUnaryRecords,
    type TagSingleConstructors,
    StripConstructor,
    ToLower,
    FirstLetterToLower,
    StripConstructorParamType,
  )
where

import Data.Aeson
  ( FromJSON (..),
    GFromJSON,
    GToJSON,
    Options (..),
    ToJSON (..),
    Zero,
    camelTo2,
    defaultOptions,
    genericParseJSON,
    genericToJSON,
  )
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import Data.Kind (Constraint, Type)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Reflection (Reifies (..))
import Data.Typeable (Typeable, typeRep, typeRepTyCon)
import GHC.Generics (Generic, Rep)
import GHC.TypeLits
  ( KnownNat,
    KnownSymbol,
    Nat,
    Symbol,
    natVal,
    symbolVal,
  )

newtype WithOptions options a = WithOptions {runWithOptions :: a}

data StrFun
  = Drop Nat
  | CamelTo2 Symbol
  | forall p. UserDefined p

type Drop = 'Drop

type CamelTo2 = 'CamelTo2

type UserDefined = 'UserDefined

type family Demoted a where
  Demoted Symbol = String
  Demoted StrFun = String -> String
  Demoted [a] = [Demoted a]
  Demoted Setting = Options -> Options
  Demoted SumEncoding' = Aeson.SumEncoding
  Demoted a = a

data SumEncoding'
  = TaggedObj {tagFieldName' :: Symbol, contentsFieldName :: Symbol}
  | UntaggedVal
  | ObjWithSingleField
  | TwoElemArr

type TaggedObj = 'TaggedObj

type UntaggedVal = 'UntaggedVal

type ObjWithSingleField = 'ObjWithSingleField

type TwoElemArr = 'TwoElemArr

data Setting
  = FieldLabelModifier [StrFun]
  | ConstructorTagModifier [StrFun]
  | AllNullaryToStringTag Bool
  | OmitNothingFields Bool
  | SumEnc SumEncoding'
  | UnwrapUnaryRecords Bool
  | TagSingleConstructors Bool

type FieldLabelModifier = 'FieldLabelModifier

type ConstructorTagModifier = 'ConstructorTagModifier

type AllNullaryToStringTag = 'AllNullaryToStringTag

type OmitNothingFields = 'OmitNothingFields

type SumEnc = 'SumEnc

type UnwrapUnaryRecords = 'UnwrapUnaryRecords

type TagSingleConstructors = 'TagSingleConstructors

class Demotable (a :: k) where
  demote :: proxy a -> Demoted k

type family All (p :: Type -> Constraint) (xs :: [Type]) :: Constraint where
  All p '[] = ()
  All p (x ': xs) = (p x, All p xs)

instance Reifies f (String -> String) => Demotable ('UserDefined f) where
  demote _ = reflect @f Proxy

instance KnownSymbol sym => Demotable sym where
  demote = symbolVal

instance (KnownSymbol s, KnownSymbol t) => Demotable ('TaggedObj s t) where
  demote _ = Aeson.TaggedObject (symbolVal @s Proxy) (symbolVal @t Proxy)

instance Demotable 'UntaggedVal where
  demote _ = Aeson.UntaggedValue

instance Demotable 'ObjWithSingleField where
  demote _ = Aeson.ObjectWithSingleField

instance Demotable 'TwoElemArr where
  demote _ = Aeson.TwoElemArray

instance Demotable xs => Demotable ('FieldLabelModifier xs) where
  demote _ o = o {fieldLabelModifier = foldr (.) id (demote (Proxy @xs))}

instance Demotable xs => Demotable ('ConstructorTagModifier xs) where
  demote _ o = o {constructorTagModifier = foldr (.) id (demote (Proxy @xs))}

instance Demotable b => Demotable ('AllNullaryToStringTag b) where
  demote _ o = o {allNullaryToStringTag = demote (Proxy @b)}

instance Demotable b => Demotable ('OmitNothingFields b) where
  demote _ o = o {omitNothingFields = demote (Proxy @b)}

instance Demotable b => Demotable ('UnwrapUnaryRecords b) where
  demote _ o = o {unwrapUnaryRecords = demote (Proxy @b)}

instance Demotable b => Demotable ('TagSingleConstructors b) where
  demote _ o = o {tagSingleConstructors = demote (Proxy @b)}

instance Demotable b => Demotable ('SumEnc b) where
  demote _ o = o {sumEncoding = demote (Proxy @b)}

instance Demotable 'True where
  demote _ = True

instance Demotable 'False where
  demote _ = False

instance KnownNat n => Demotable ('Drop n) where
  demote _ = drop (fromIntegral $ natVal (Proxy :: Proxy n))

instance KnownSymbol sym => Demotable ('CamelTo2 sym) where
  demote _ = camelTo2 $ head $ symbolVal @sym Proxy

instance {-# OVERLAPPING #-} Demotable ('[] :: [k]) where
  demote _ = []

instance (Demotable (x :: k), Demotable (xs :: [k])) => Demotable (x ': xs) where
  demote _ = demote (Proxy @x) : demote (Proxy @xs)

type DefaultOptions = ('[] :: [Setting])

reflectOptions :: forall xs proxy. Demotable (xs :: [Setting]) => proxy xs -> Options
reflectOptions pxy = foldr ($) defaultOptions (demote pxy)

instance (Demotable (options :: [Setting])) => Reifies options Options where
  reflect = reflectOptions

instance
  (Generic a, GToJSON Zero (Rep a), Reifies (options :: k) Options) =>
  ToJSON (WithOptions options a)
  where
  toJSON = genericToJSON (reflect (Proxy @options)) . runWithOptions

instance
  (Generic a, GFromJSON Zero (Rep a), Reifies (options :: k) Options) =>
  FromJSON (WithOptions options a)
  where
  parseJSON = fmap WithOptions . genericParseJSON (reflect (Proxy @options))

data StripConstructor a

instance Typeable a => Reifies (StripConstructor a) (String -> String) where
  reflect _ = \s ->
    let (head : tail) = show (typeRep (Proxy @a))
     in fromMaybe s $ stripPrefix (toLower head : tail) s

data ToLower

instance Reifies ToLower (String -> String) where
  reflect _ = map toLower

data FirstLetterToLower

instance Reifies FirstLetterToLower (String -> String) where
  reflect _ = \(h : t) -> toLower h : t

data StripConstructorParamType a

instance Typeable a => Reifies (StripConstructorParamType a) (String -> String) where
  reflect _ = \s ->
    let (head : tail) = show $ typeRepTyCon (typeRep (Proxy @a))
     in fromMaybe s $ stripPrefix (toLower head : tail) s
