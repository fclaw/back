{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Telegram (Service (..), mkService, TelegramMsg (..), runMsgDeliver, File (..)) where

import Control.Applicative
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Catch
import qualified Data.ByteString.Lazy as BL
import Data.Foldable
import Data.Functor
import Data.Maybe (fromMaybe, isNothing)
import Data.String.Conv
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Typeable
import Katip
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.MultipartFormData (PartM (partFilename), formDataBody, partBS, partLBS)
import qualified Network.HTTP.Types.Status as HTTP
import Scaffold.Config (Env (..), Telegram (..))
import Servant.Multipart.File

data Service = Service
  { sendMsg :: (Severity -> LogStr -> IO ()) -> T.Text -> IO (),
    sendFile :: (Severity -> LogStr -> IO ()) -> File -> IO ()
  }

data TelegramMsg = forall a. Typeable a => TelegramMsg String a

mkService :: HTTP.Manager -> Telegram -> IO Service
mkService _ tl
  | isNothing (telegramBot tl) =
      return $
        Service
          { sendMsg = \logger _ -> void $ logger InfoS "telegram bot isn't set.skip",
            sendFile = \logger _ -> void $ logger InfoS "telegram bot isn't set.skip"
          }
mkService mgr Telegram {..} = do
  let bot = fromMaybe undefined telegramBot
  let url_msg = telegramHost <> bot <> "/sendMessage"
  let url_file = telegramHost <> bot <> "/sendDocument"
  req_msg <- HTTP.parseRequest $ T.unpack url_msg
  req_file <- HTTP.parseRequest $ T.unpack url_file
  let sendText logger msg =
        catch @IO @HTTP.HttpException
          ( when (telegramEnv == Dev) $
              for_ (splitByteString (toS msg)) $ \chunk -> do
                void $ logger DebugS (ls ("telegram req: " <> chunk))
                response <-
                  flip HTTP.httpLbs mgr $
                    HTTP.urlEncodedBody
                      [ ("chat_id", toS telegramChat),
                        ("text", "`" <> toS chunk <> "`"),
                        ("parse_mode", "markdown")
                      ]
                      req_msg {HTTP.method = "POST"}
                let response_status = HTTP.statusCode $ HTTP.responseStatus response
                let response_body = toS $ HTTP.responseBody response
                let log_msg =
                      "telegram response with status "
                        <> show response_status
                        <> ": "
                        <> response_body
                void $ logger DebugS (ls ("telegram resp: " <> log_msg))
          )
          $ \e -> void $ logger ErrorS $ ls (show e)
  return $
    Service
      { sendMsg = sendText,
        sendFile = \logger file@File {..} ->
          E.catches
            ( when (telegramEnv == Dev) $ do
                content <- BL.readFile filePath
                req <-
                  formDataBody
                    [ partBS "chat_id" (encodeUtf8 ("@" <> telegramChat)),
                      (partLBS "document" content) {partFilename = Just (T.unpack fileName)}
                    ]
                    req_file {HTTP.method = "POST"}
                response <- HTTP.httpLbs req mgr
                let response_status = HTTP.statusCode $ HTTP.responseStatus response
                let response_body = toS $ HTTP.responseBody response
                let log_msg =
                      "telegram response with status "
                        <> show response_status
                        <> ": "
                        <> response_body
                void $ logger DebugS (ls ("telegram resp: " <> log_msg))
            )
            [ E.Handler $ \(e :: HTTP.HttpException) -> void $ logger ErrorS $ ls (show e),
              E.Handler $ \(e :: E.IOException) -> do
                sendText logger $ T.pack (show (e, file))
                void $ logger ErrorS $ ls (show (e, file))
            ]
      }
  where
    splitByteString =
      let split xs source
            | BL.length source < 4096 = source : xs
          split xs old =
            let (x, new) = BL.splitAt 4096 old
             in split (x : xs) new
       in reverse . split []

runMsgDeliver :: TChan TelegramMsg -> Service -> (Severity -> LogStr -> IO ()) -> IO ()
runMsgDeliver ch service log = atomically (readTChan ch) >>= sequence_ . (`telegramMsgEliminator` run service log)

telegramMsgEliminator :: forall r. TelegramMsg -> (forall a. Typeable a => a -> r) -> r
telegramMsgEliminator (TelegramMsg _ msg) f = f msg

run :: forall a. Typeable a => Service -> (Severity -> LogStr -> IO ()) -> a -> Maybe (IO ())
run Service {..} log val =
  msum
    [ cast @_ @T.Text val <&> sendMsg log,
      cast @_ @File val <&> sendFile log
    ]
    <|> pure
      ( sendMsg
          log
          ( "unknown telegram type "
              <> T.pack (show (typeRep (Proxy @a)))
          )
      )
