module Language.Haskell.LSP.TH.Constants where

import           Data.Char
import           Data.Aeson.TH
import           Control.Lens.TH (makeLensesWith, abbreviatedFields)
import           Language.Haskell.TH

-- ---------------------------------------------------------------------

-- | Standard options for use when generating JSON instances
lspOptions :: Options
lspOptions = defaultOptions { omitNothingFields = True, fieldLabelModifier = defaultModifier }
 -- NOTE: This needs to be in a separate file because of the TH stage restriction

makeFieldsNoPrefix :: Name -> DecsQ
makeFieldsNoPrefix = makeLensesWith abbreviatedFields

defaultModifier :: String -> String
defaultModifier str = f (dropWhile (not . isUpper) str)
  where f (x:xs) = toLower x : xs
        f [] = []

customModifier :: String -> String
customModifier xs =
  case defaultModifier xs of
    "xdata" -> "data"
    "xtype" -> "type"
    xs'     -> xs'
