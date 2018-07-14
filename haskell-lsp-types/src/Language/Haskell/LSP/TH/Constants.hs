
module Language.Haskell.LSP.TH.Constants where

import           Data.Aeson.TH

-- ---------------------------------------------------------------------

-- | Standard options for use when generating JSON instances
lspOptions :: Options
lspOptions = defaultOptions { omitNothingFields = True, fieldLabelModifier = defaultModifier }
 -- NOTE: This needs to be in a separate file because of the TH stage restriction

defaultModifier :: String -> String
defaultModifier = drop 1 . dropWhile (/= '_')

customModifier :: String -> String
customModifier xs =
  case defaultModifier xs of
    "xdata" -> "data"
    "xtype" -> "type"
    xs'     -> xs'

