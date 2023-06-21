{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.Servant.Logger (logMw) where

import Network.Wai
import KatipController
import System.CPUTime
import Katip
import Text.Printf
import Control.Lens.Iso.Extended
import Control.Lens
import Katip.Core (getLoc)

logMw :: KatipLoggerLocIO -> Middleware
logMw log app req runResp =
  app req $ \resp -> do
    start <- getCPUTime
    received <- runResp resp
    end <- getCPUTime
    let mills = fromIntegral @_ @Double (end - start) * 1e-12
    let duration = printf "duration: %.2f sec" mills
    let message =
         "response.http.status: " <>
         show (responseStatus resp) <>
         ", request.http.rawPathInfo: " <>
         (rawPathInfo req^.from textbs.from stext) <>
         ", " <> duration
    log getLoc InfoS (ls message)
    return received
