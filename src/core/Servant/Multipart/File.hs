{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Multipart.File (Files (..), File (..)) where

import Control.Lens
import Data.Proxy
import Data.Swagger as Swagger
import qualified Data.Text as T
import Data.Typeable
import Servant.API
import Servant.Multipart
import Servant.Swagger
import qualified Servant.Swagger.Internal

newtype Files = Files {filesXs :: [File]} deriving (Show)

data File = File
  { fileName :: !T.Text,
    fileMime :: !T.Text,
    filePath :: !FilePath
  }
  deriving (Show, Typeable)

instance FromMultipart Tmp Files where
  fromMultipart x =
    Right $ Files $ flip map (files x) $ \FileData {..} ->
      File fdFileName fdFileCType fdPayload

instance FromMultipart Tmp File where
  fromMultipart x = mkFile <$> lookupFile "payloadFile" x
    where
      mkFile FileData {..} = File fdFileName fdFileCType fdPayload

instance (Typeable a, HasSwagger sub) => HasSwagger (MultipartForm tag a :> sub) where
  toSwagger _ =
    toSwagger (Proxy :: Proxy sub)
      & Servant.Swagger.Internal.addParam paramFile
    where
      paramFile =
        mempty
          & Swagger.name .~ ("payload" <> T.pack (show (typeRep (Proxy @a))))
          & Swagger.required ?~ True
          & Swagger.schema
            .~ Swagger.ParamOther
              ( mempty
                  & Swagger.in_ .~ Swagger.ParamFormData
                  & Swagger.paramSchema
                    .~ ( mempty
                           & Swagger.type_
                             ?~ Swagger.SwaggerFile
                       )
              )
