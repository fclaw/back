{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.Error.ParseFailure (formatter) where

import Data.Aeson
import Data.Proxy (Proxy (..))
import Data.String.Conversions (cs)
import Network.HTTP.Types (hContentType)
import Servant.API.ContentTypes (handleAcceptH)
import Servant.API.Extended (JSON)
import Servant.Server (err400, errBody, errHeaders, getAcceptHeader)
import Servant.Server.Internal.ErrorFormatter

formatter :: ErrorFormatter
formatter tr req err =
  let -- aeson Value which will be sent to the client
      value = object ["combinator" .= show tr, "error" .= err]
      -- Accept header of the request
      accH = getAcceptHeader req
   in -- handleAcceptH is Servant's function that checks whether the client can accept a
      -- certain message type.
      -- In this case we call it with "Proxy '[JSON]" argument, meaning that we want to return a JSON.
      case handleAcceptH (Proxy :: Proxy '[JSON]) accH value of
        -- If client can't handle JSON, we just return the body the old way
        Nothing -> err400 {errBody = cs err}
        -- Otherwise, we return the JSON formatted body and set the "Content-Type" header.
        Just (ctypeH, body) ->
          err400
            { errBody = body,
              errHeaders = [(hContentType, cs ctypeH)]
            }
