{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Network.Wai.Middleware.Servant.Logger (logMw) where

import Control.Lens
import Control.Lens.Iso.Extended
import Katip
import Katip.Core (getLoc)
import KatipController
import Network.Wai
import System.CPUTime
import Text.Printf

logMw :: KatipLoggerLocIO -> Middleware
logMw log app req runResp =
  app req $ \resp -> do
    start <- getCPUTime
    received <- runResp resp
    end <- getCPUTime
    let mills = fromIntegral @_ @Double (end - start) * 1e-12
    let duration = printf "duration: %.2f sec" mills
    let message =
          "response.http.status: "
            <> show (responseStatus resp)
            <> ", request.http.rawPathInfo: "
            <> (rawPathInfo req ^. from textbs . from stext)
            <> ", "
            <> duration
    log getLoc InfoS (ls message)
    return received
