module Eitri.Compiler where

import Prelude

import Data.Bifunctor ( first )
import Data.List ( intercalate )
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Monoid ((<>))
import qualified Data.Set as Set
import Data.Text ( Text, unpack, pack )

import Text.Megaparsec ( parse, Dec, Token, ParseError(..), SourcePos(..), unPos, Pos, showErrorComponent )

import Eitri.AST ( Statement )
import Eitri.CodeGen ( genCpp )
import Eitri.Parser ( statement )
import Eitri.Tokeniser ( tokenise, TokenStream )

compile :: Text -> Either String Statement
compile src = do
  tokens <- first show $ tokenise src
  first (formatParseError src) $ parse statement noFile tokens
    where noFile = ""

maybeFromRight :: Either a b -> Maybe b
maybeFromRight ( Right b ) = Just b
maybeFromRight ( Left  _ ) = Nothing

pToI :: Pos -> Int
pToI = fromIntegral . unPos

formatParseError :: Text -> ParseError (Token TokenStream) Dec -> String
formatParseError src (ParseError ((SourcePos _ (pToI -> line) (pToI -> col)) :| _) unexpected _ _) =
  "Error on line " <> show line <> ", col " <> show col <> ": unexpected "<> showUnexpected <> "\n\n" <> srcLine line <> "\n" <> caret col
    where srcLine i = lines (unpack src) !! (i - 1) -- line counts are 1-based, list is 0-based
          caret i = replicate (i-1) ' ' ++ "^" -- col counts are 1-based, sub 1 for the caret
          showUnexpected = intercalate ", " $ showErrorComponent <$> Set.toList unexpected

compileCpp :: String -> String
compileCpp = either id (show . genCpp) . compile . pack
