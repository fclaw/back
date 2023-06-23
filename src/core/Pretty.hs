module Pretty (mkPretty, pPrint) where

import Control.Lens (from, to, (^.))
import Control.Lens.Iso.Extended (stext)
import Data.Text.Lazy (toStrict)
import Text.Pretty.Simple
import Text.Pretty.Simple.Internal.Printer

mkPretty :: Show a => String -> a -> String
mkPretty msg x = msg ++ "\n" ++ pStringOpt defaultOutputOptionsNoColor (escapeNonPrintable (show x)) ^. to toStrict . from stext
