{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Scaffold.Api.Controller.File.Patch (controller) where

-- import Data.Aeson.Unit

import BuildInfo
import Control.Lens
import Control.Lens.Iso.Extended
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Data.Coerce
import Data.Either.Combinators
import Data.Int
import Data.Traversable
import Database.Transaction
import Katip
import KatipController
import Network.Minio hiding (Bucket)
import Scaffold.Statement.File as File
import Scaffold.Transport.Id
import Scaffold.Transport.Model.File
import Scaffold.Transport.Response

controller :: Id "file" -> File -> KatipControllerM (Response ())
controller id file = do
  runTelegram $location (id, file)
  $(logTM) DebugS (logStr (show (id, file)))
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  let notFound = "file {" <> show (coerce @(Id "file") @Int64 id) ^. stext <> "} not found"
  resp <-
    fmap (maybeToRight (asError notFound)) $
      katipTransaction hasql $
        statement File.getHashWithBucket id
  let patch (hash, bucket) = do
        Minio {..} <- fmap (^. katipEnv . minio) ask
        void $
          katipTransaction hasql $
            statement
              File.patch
              ( Name (UnicodeText (fileName file)),
                Mime (UnicodeText (fileMime file)),
                hash
              )
        minioRes <-
          liftIO $
            runMinioWith minioConn $
              fPutObject
                (minioBucketPrefix <> "." <> coerce @Bucket bucket)
                (coerce hash)
                (filePath file)
                defaultPutObjectOptions
        whenLeft minioRes $ \e -> do
          $(logTM) ErrorS (logStr (show e))
          throwError undefined
        return ()
  fromEither <$> for resp patch
