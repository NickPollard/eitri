{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Newlang.Parser (
) where

import Control.Applicative (many, (<|>), liftA2)
import Data.Bifunctor (second)
import qualified Data.List as List
import Data.List.NonEmpty
import Data.Loc (L(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Text.Megaparsec.Combinator
import Text.Megaparsec.Error
import Text.Megaparsec.Prim

import Newlang.AST
import Newlang.Tokeniser (Tok)
import qualified Newlang.Tokeniser as Tok
import qualified Newlang.Tokeniser as T

--type Parser = Parsec [L Token] ()
newtype TokenStream = TokenStream [L Tok]

instance Stream TokenStream where
  type Token TokenStream = L Tok
  uncons (TokenStream l) = second TokenStream <$> List.uncons l
  updatePos = undefined -- TODO

type MonadParser m = MonadParsec Dec TokenStream m

type' :: MonadParser m => m Type
type' = undefined -- TODO

varDef :: MonadParser m => m SomeVarDef
varDef = var <|> val
  where var = fmap SomeVarDef $ Var <$> (reserved "var" *> name) <* reserved ":" <*> type'
        val = fmap SomeVarDef $ Val <$> (reserved "val" *> name) <* reserved ":" <*> type'

-- assignment: val name = expr
statement :: MonadParser m => m Statement
statement =   Assign <$> name <* reserved "=" <*> expr
          <|> Eval <$> expr
          <|> Initialize <$> varDef <* reserved "=" <*> expr
          <|> FunDef <$> (reserved "def" *> name) <*> parens (many argDef) <* reserved "=" <*> block

parens :: MonadParser m => m a -> m a
parens p = reserved "(" *> p <* reserved ")"

argDef :: MonadParser m => m (VarName, Type)
argDef = (name <* reserved ":") `tuple` type'

tuple :: Applicative f => f a -> f b -> f (a,b)
tuple = liftA2 (,)

block :: MonadParser m => m [Statement]
block = undefined -- TODO

expr :: MonadParser m => m Expr
expr =     (Apply <$> name <*> many expr)
       <|> (ValueOf <$> name)

name :: MonadParser m => m Name
name = token test Nothing
  where test (L _ (Tok.Name t)) = Right $ Name t
        test other              = didntExpect other

didntExpect :: (ErrorComponent e) => t -> Either (Set (ErrorItem t), Set (ErrorItem t), Set e) a
didntExpect a = Left (Set.singleton (Tokens $ a :| []), Set.empty, Set.empty)

reserved :: MonadParser m => Text -> m ()
reserved word = token test Nothing
  where test (L _ (Tok.ReservedWord w)) | word == w = Right ()
        test other = didntExpect other

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
