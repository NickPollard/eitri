module Newlang.Parser (
) where

import Data.Loc (L(..))
import Data.Text (Text, pack)
import Text.Parsec

import Newlang.AST
import Newlang.Tokeniser (Token)
import qualified Newlang.Tokeniser as T

type Parser = Parsec [L Token] ()


-- assignment: val name = expr
--assign      = Assign     <$> name <* string "=" <*> expr

--expr :: Parser Expr
--expr = --applyFn | valueOf
      --ValueOf <$> name

--name :: Parser Name
--name = parseToken match
  --where match (Name t) = Just t
        --match _        = Nothing

--parseToken :: (L Token -> Maybe a) -> Parser a
--parseToken = token show loc


{-

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

-}
