{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import BuildInfo (gitCommit)
import qualified Cfg.Github as Github
import qualified Cfg.SendGrid as SendGrid
import Control.Applicative ((<|>))
import Control.Exception
import Control.Lens hiding (Unwrapped, Wrapped)
import Control.Lens.Iso.Extended
import Control.Monad
import Control.Monad.RWS.Strict (evalRWST)
import Data.Char (isUpper, toLower)
import Data.Coerce (coerce)
import Data.Default.Class
import Data.Foldable (for_)
import qualified Data.Map as Map
import qualified Data.Map.Lazy as MapLazy
import Data.Maybe
import Data.Monoid.Colorful (hGetTerm)
import qualified Data.Pool as Pool
import Data.String
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock.System
import Data.Traversable (for)
import GHC.Read
import qualified Hasql.Connection as HasqlConn
import Katip
import Katip.Scribes.Minio (mkMinioScribe)
import KatipController
import Network.HTTP.Client
  ( ManagerSettings
      ( managerConnCount,
        managerResponseTimeout
      ),
    responseTimeoutMicro,
  )
import qualified Network.HTTP.Client.TLS as Http
import qualified Network.Minio as Minio
import Options.Generic
import Pretty
import qualified Scaffold.Application as App
import Scaffold.Auth (User (User))
import Scaffold.Config
import Scaffold.EnvKeys
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix
import System.IO
import qualified Telegram
import Text.ParserCombinators.ReadPrec (pfail)
import qualified Text.Read.Lex as L

data PrintCfg = Y | N deriving stock (Generic)

instance Show PrintCfg where
  show Y = "y"
  show N = "n"

instance Read PrintCfg where
  readPrec =
    parens
      ( do
          L.Ident s <- lexP
          case s of
            "y" -> return Y
            "n" -> return N
            _ -> pfail
      )

instance ParseField PrintCfg

data Cmd w = Cmd
  { cfgPath :: w ::: FilePath <?> "config file path",
    localhost :: w ::: Maybe String <?> "override db host if needed, used along with port",
    localport :: w ::: Maybe Int <?> "override db port if needed",
    pathToKatip :: w ::: Maybe FilePath <?> "path to katip log",
    pathToJwk :: w ::: Maybe FilePath <?> "path to jwk",
    minioHost :: w ::: Maybe String <?> "minio host",
    minioPort :: w ::: Maybe String <?> "minio port",
    swaggerHost :: w ::: Maybe String <?> "swagger host",
    swaggerPort :: w ::: Maybe Int <?> "swagger port",
    serverPort :: w ::: Maybe Int <?> "server port",
    cfgAdminStoragePath :: w ::: FilePath <?> "admin storage",
    printCfg :: w ::: Maybe PrintCfg <?> "whether config be printed",
    envPath :: w ::: Maybe FilePath <?> "file for storing sensitive data. it's used only in deployment",
    mute500 :: w ::: Maybe Bool <?> "how to render 500 error"
  }
  deriving stock (Generic)

deriving instance Show (Cmd Unwrapped)

instance ParseRecord (Cmd Wrapped) where
  parseRecord =
    parseRecordWithModifiers
      defaultModifiers
        { fieldNameModifier = toSnake
        }

-- |
--   Convert CamelCased or mixedCases 'String' to a 'String' with underscores,
--   the \"snake\" 'String'.
--   It splits an input value to chunks by 'isUpper' predicate,
--   then adds underscores to each element except the first.
--   Finally concats the result and convers it downcase.
toSnake :: String -> String
toSnake = map toLower . concat . underscores . splitR isUpper
  where
    underscores [] = []
    underscores (h : t) = h : map ('_' :) t
    splitR _ [] = []
    splitR p s =
      let go m s' =
            case break p s' of
              (b', []) -> [m : b']
              (b', x : xs) -> (m : b') : go x xs
       in case break p s of
            (b, []) -> [b]
            ([], h : t) -> go h t
            (b, h : t) -> b : go h t

main :: IO ()
main = do
  cmd@Cmd {..} <- unwrapRecord "scaffold"
  print "------ Cmd: start ------"
  pPrint cmd
  print "------ Cmd: end ------"

  -- at this initialisation step we have to obtain sensitive data from env
  envKeys <- fmap join $ for envPath $ \path -> do
    cond <- doesFileExist path
    if cond
      then fmap Just $ Scaffold.Config.load @EnvKeys path
      else return Nothing

  print "------ EnvKeys: start ------"
  pPrint envKeys
  print "------ EnvKeys: end ------"

  rawCfg <- Scaffold.Config.load @Scaffold.Config.Config cfgPath
  let cfg =
        rawCfg
          & db . host %~ (`fromMaybe` localhost)
          & db . port %~ (`fromMaybe` localport)
          & katip . path %~ (\path -> maybe path (</> path) pathToKatip)
          & Scaffold.Config.minio . host %~ (`fromMaybe` minioHost)
          & Scaffold.Config.minio . port %~ (`fromMaybe` minioPort)
          & swagger . host %~ (`fromMaybe` swaggerHost)
          & swagger . port %~ (flip (<|>) swaggerPort)
          & serverConnection . port %~ (`fromMaybe` serverPort)
          & Scaffold.Config.sendGrid . apiKey %~ (flip (<|>) (join $ fmap envKeysSendgrid envKeys))

  for_ printCfg $
    \case
      Y ->
        do
          print "------ Cfg: start ------"
          pPrint cfg
          print "------ Cfg: end ------"
      N -> pure ()

  -- at this initialisation step we have to put sensitive data into the config

  term <- hGetTerm stdout
  hSetBuffering stdout NoBuffering

  let mkRawConn x =
        HasqlConn.settings
          (x ^. host . stext . textbs)
          (x ^. port . to fromIntegral)
          (x ^. Scaffold.Config.user . stext . textbs)
          (x ^. pass . stext . textbs)
          (x ^. database . stext . textbs)

  hasqlpool <-
    Pool.createPool
      ( HasqlConn.acquire (mkRawConn (cfg ^. db))
          >>= either (throwIO . ErrorCall . maybe "hasql connection error" (^. from textbs . from stext)) pure
      )
      HasqlConn.release
      (cfg ^. hasql . poolN)
      (cfg ^. hasql . tm)
      (cfg ^. hasql . resPerStripe)

  std <-
    mkHandleScribeWithFormatter
      jsonFormat
      ColorIfTerminal
      stdout
      (permitItem (cfg ^. katip . severity . from stringify))
      (cfg ^. katip . verbosity . from stringify)
  tm <- fmap systemSeconds getSystemTime
  let katipFilePath = cfg ^. katip . path <> "/" <> show tm <> ".log"
  createDirectoryIfMissing True $ cfg ^. katip . path
  fileHdl <- openFile katipFilePath AppendMode

  mapM_ (`hSetEncoding` utf8) [stdout, stderr, fileHdl]

  file <-
    mkHandleScribe
      (ColorLog True)
      fileHdl
      (permitItem (cfg ^. katip . severity . from stringify))
      (cfg ^. katip . verbosity . from stringify)
  let mkNm = Namespace [("<" ++ $(gitCommit) ++ ">") ^. stext]
  init_env <- initLogEnv mkNm (cfg ^. katip . Scaffold.Config.env . isoEnv . stext . coerced)

  createDirectoryIfMissing True cfgAdminStoragePath
  admin_storage <- withFile (cfgAdminStoragePath <> "/" <> "passwords") ReadMode $ \h -> do
    content <- T.hGetContents h
    return $
      Map.fromList $
        flip foldMap (T.splitOn "," content) $ \x ->
          case T.splitOn ":" x of
            [pass, email] -> [(pass, User email)]
            _ -> []

  let appCfg =
        App.Cfg
          (cfg ^. swagger . host . coerced)
          (cfg ^. swagger . port)
          (cfg ^. serverConnection . port)
          (cfg ^. cors)
          (cfg ^. serverError)
          admin_storage
          mute500

  manager <-
    Http.newTlsManagerWith
      Http.tlsManagerSettings
        { managerConnCount = 1,
          managerResponseTimeout =
            responseTimeoutMicro (5 * 10 ^ 6)
        }

  minioEnv <-
    flip Minio.mkMinioConn manager $
      Minio.setCreds
        ( Minio.Credentials
            (cfg ^. Scaffold.Config.minio . accessKey)
            (cfg ^. Scaffold.Config.minio . secretKey)
        )
        (fromString (cfg ^. Scaffold.Config.minio . host <> ":" <> cfg ^. Scaffold.Config.minio . port))

  telegram <- Telegram.mkService manager (cfg ^. Scaffold.Config.telegram & bot %~ (flip (<|>) (join $ fmap envKeysTelegramBot envKeys)))

  minioScribe <-
    mkMinioScribe
      minioEnv
      (cfg ^. Scaffold.Config.minio . logBucket . stext)
      (permitItem (cfg ^. katip . severity . from stringify))
      (cfg ^. katip . verbosity . from stringify)

  let env = do
        env' <- registerScribe "stdout" std defaultScribeSettings init_env
        env'' <- registerScribe "file" file defaultScribeSettings env'
        registerScribe "minio" minioScribe defaultScribeSettings env''

  let s@Scaffold.Config.SendGrid {..} = cfg ^. Scaffold.Config.sendGrid
  let sendgrid = fmap ((s,) . SendGrid.configure sendGridUrl) sendGridApiKey

  let github = do
        k <- envKeys
        creds_old <- fmap coerce $ envKeysGithub k
        let cred_new = flip MapLazy.map creds_old $ \x -> (Github.configure (Scaffold.EnvKeys.key x), x)
        return cred_new

  let captcha = envKeys >>= envKeysCaptchaKey

  let katipMinio = Minio minioEnv (cfg ^. Scaffold.Config.minio . Scaffold.Config.bucketPrefix)
  let katipEnv = KatipEnv term hasqlpool manager (cfg ^. service . coerced) katipMinio telegram sendgrid github captcha

  let runApp le = runKatipContextT le (mempty @LogContexts) mempty $ App.run appCfg
  bracket env closeScribes $ void . (\x -> evalRWST (App.runAppMonad x) katipEnv def) . runApp
