module Newlang.Tokeniser (
  Token,
  tokenise
) where

import Data.Functor.Identity (Identity)
--import Data.Loc (L)
import Data.Text (Text, pack)
import Text.Megaparsec hiding (Token)
import qualified Text.Megaparsec as MP
--import Text.Megaparsec.Text

data Token = Name Text
           | ReservedWord Text
           | Symbol Text
           deriving Show

reservedWords = "def"
         : "return"
         : "if"
         : "then"
         : "else"
         : "case"
         : []

symbol' = "("
       : ")"
       : "["
       : "]"
       : "}"
       : "{"
       : ":"
       : "="
       : []

type Parser = ParsecT
              Dec       -- MegaParsec Default ErrorComponent
              Text      -- Parse from a `Text` stream
              Identity  -- runs in `Identity` Monad

txt :: String -> Parser Text
txt x = pack <$> string x

name' = pack <$> ((:) <$> alphaNumChar <*> many alphaNumChar)
symbols = choice $ txt <$> symbol'
reserved = choice $ txt <$> reservedWords

withSpaces = flip endBy space

tokeniser :: Parser [Token]
tokeniser = withSpaces $ ReservedWord <$> reserved
                     <|> Symbol <$> symbols
                     <|> Name <$> name'

tokenise :: Text -> Either (ParseError (MP.Token Text) Dec)  [Token]
tokenise = parse (tokeniser <* eof) noFile
  where noFile = ""
