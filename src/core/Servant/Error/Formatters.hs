module Servant.Error.Formatters (formatters) where

import qualified Servant.Error.ParseFailure as ParseFailure
import qualified Servant.Error.RouteNotFound404 as RouteNotFound404
import Servant.Server.Internal.ErrorFormatter

formatters :: ErrorFormatters
formatters =
  defaultErrorFormatters
    { notFoundErrorFormatter = RouteNotFound404.formatter,
      bodyParserErrorFormatter = ParseFailure.formatter
    }
