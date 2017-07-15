{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Newlang.Tokeniser (
  Tok(..),
  tokenise
) where

import Data.Functor.Identity (Identity)
--import Data.Loc (L)
import Data.Text (Text, pack)
import Text.Megaparsec
import Text.Megaparsec.Prim (MonadParsec)
import qualified Text.Megaparsec as MP
--import Text.Megaparsec.Text

data Tok = Name Text
           | ReservedWord Text
           | Symbol Text
           deriving (Show, Eq, Ord)

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

{-
type Parser = ParsecT
              Dec       -- MegaParsec Default ErrorComponent
              Text      -- Parse from a `Text` stream
              Identity  -- runs in `Identity` Monad
              -}

type MonadParser m = MonadParsec Dec Text m

txt :: MonadParser m => String -> m Text
txt x = pack <$> string x

name' :: MonadParser m => m Text
name' = pack <$> ((:) <$> alphaNumChar <*> many alphaNumChar)

symbols :: MonadParser m => m Text
symbols = choice $ txt <$> symbol'

reserved :: MonadParser m => m Text
reserved = choice $ txt <$> reservedWords

withSpaces :: MonadParser m => m a -> m a
withSpaces p = p <* space

someToken :: MonadParser m => m Tok
someToken = withSpaces $ ReservedWord <$> reserved
                     <|> Symbol <$> symbols
                     <|> Name <$> name'

tokeniser :: MonadParser m => m [Tok]
tokeniser = many someToken

tokenise :: Text -> Either (ParseError (Token Text) Dec)  [Tok]
tokenise = parse (tokeniser <* eof) noFile
  where noFile = ""
