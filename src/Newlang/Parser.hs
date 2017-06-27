module Newlang.Parser (
  tokenise
) where

import Data.Text (Text, pack)
import Text.Parsec
--import Text.Parsec.Char
import Text.Parsec.Text

{-

-- assignment: val name = expr

(~~>) :: Functor f => f a -> (a -> b) -> f b
(~~>) = flip fmap

--(~) :: Applicative f => f a

-- a: Int
variableDef :: Parser (Name :- Type)
eval :: Parser Statement
functionDef :: Parser Statement
block :: Parser Statement

initialize  = Initialize ~: vardef ~ "=" ~ expr
assign      = Assign     ~: name ~ "=" ~ expr
eval        = Eval       ~: expr
functionDef = FunDef     ~: "def" ~ name ~ "(" ~ argDefs ~ ")" ~ "=" ~ block -- def foo(a : Int) = mul(a,2)
block       = Block      ~: many statement

statement :: Parser Statement
statement = initialize
          | assign
          | eval
          | functionDef
          | block

type Statement = initialise :> assign :> block :> eval

class Parsable a where
  p :: Parser a

instance (Parsable a, Parsable b) => Parsable (a :> b) where
  p = p <:> p

initialize :: Parser Statement
initialize = VarDef ~: varDef ~ "=" ~ expr

expr :: Parser Expression
expr = applyFn | valueOf

name :: Parser Name
name = undefined

-}

----

data Token = Name Text
           | ReservedWord Text
           | Symbol Text
           deriving Show

{-
instance Show Token where
  show _ = ""
  -}

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

txt :: String -> Parser Text
txt x = pack <$> string x

name' = pack <$> many1 alphaNum
symbols = choice $ txt <$> symbol'
reserved = choice $ txt <$> reservedWords

-- TODO - handle whitespace

withSpaces = flip endBy spaces

tokeniser :: Parser [Token]
tokeniser = withSpaces $ ReservedWord <$> reserved
                     <|> Symbol <$> symbols
                     <|> Name <$> name'

tokenise :: Text -> Either ParseError [Token]
tokenise = parse (tokeniser <* eof) ""
