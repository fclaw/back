{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Katip.Scribes.Minio (mkMinioScribe) where

import Control.Concurrent.MVar
import Control.Exception (bracket_)
import Control.Lens
import Control.Lens.Iso.Extended
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text (Text, unpack)
import Hash (mkHash)
import Katip
import Network.Minio
import System.Directory
import System.FilePath

mkMinioScribe :: MinioConn -> Text -> PermitFunc -> Verbosity -> IO Scribe
mkMinioScribe conn bucket permitF verb = do
  lock <- newMVar ()
  let logger item = do
        bracket_ (takeMVar lock) (putMVar lock ()) $
          void $
            runMinioWith conn $ do
              (hash, path) <- liftIO $ do
                let hash = mkHash (item ^. itemTime)
                tmp <- getTemporaryDirectory
                let path = tmp </> unpack hash
                writeFile path $ encodePretty (itemJson verb item) ^. from textbsl . from stext
                return (hash, path)
              exists <- bucketExists bucket
              unless exists $ makeBucket bucket Nothing
              fPutObject bucket hash path defaultPutObjectOptions
  let finalize = return ()
  return $ Scribe logger finalize permitF
