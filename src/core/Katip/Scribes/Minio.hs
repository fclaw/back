{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Katip.Scribes.Minio (mkMinioScribe) where

import Katip 
import Network.Minio
import Data.Text (unpack)
import Hash (mkHash)
import Control.Exception (bracket_)
import Control.Concurrent.MVar
import System.Directory
import System.FilePath
import Control.Monad.IO.Class
import Control.Monad
import Control.Lens
import Data.Text (Text) 

mkMinioScribe :: MinioConn -> Text -> PermitFunc -> Verbosity -> IO Scribe
mkMinioScribe conn bucket permitF verb = do 
  lock <- newMVar ()
  let logger item = do
        bracket_ (takeMVar lock) (putMVar lock ()) $ 
          void $ runMinioWith conn $ do 
            (hash, path) <- liftIO $ do
               let hash = mkHash (item^.itemTime)
               tmp <- getTemporaryDirectory
               let path = tmp </> unpack hash
               writeFile path $ show $ itemJson verb item
               return (hash, path)
            exists <- bucketExists bucket
            unless exists $ makeBucket bucket Nothing   
            fPutObject bucket hash path defaultPutObjectOptions
  let finalize = return ()       
  return $ Scribe logger finalize permitF
