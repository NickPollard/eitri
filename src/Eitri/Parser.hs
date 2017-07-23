module Eitri.Parser (
  statement
) where

import Control.Applicative (many, (<|>), liftA2)
import Data.List.NonEmpty
import Data.Loc (L(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Text.Megaparsec.Error
import Text.Megaparsec.Prim

import Eitri.AST
import Eitri.Tokeniser (TokenStream)
import qualified Eitri.Tokeniser as Tok

type MonadParser m = MonadParsec Dec TokenStream m

type' :: MonadParser m => m Type
type' = name

varDef :: MonadParser m => m SomeVarDef
varDef = var <|> val
  where var = fmap SomeVarDef $ Var <$> (reserved "var" *> name) <* symbol ":" <*> type'
        val = fmap SomeVarDef $ Val <$> (reserved "val" *> name) <* symbol ":" <*> type'

-- assignment: val name = expr
statement :: MonadParser m => m Statement
statement =   try (Assign <$> name <* symbol "=" <*> expr)
          <|> Initialize <$> varDef <* symbol "=" <*> expr
          <|> FunDef <$> (reserved "def" *> name) <*> parens (many argDef) <* symbol "=" <*> block
          <|> Eval <$> expr

braces :: MonadParser m => m a -> m a
braces p = symbol "{" *> p <* symbol "}"

parens :: MonadParser m => m a -> m a
parens p = symbol "(" *> p <* symbol ")"

argDef :: MonadParser m => m (VarName, Type)
argDef = (name <* symbol ":") `tuple` type'

block :: MonadParser m => m [Statement]
block = braces $ many statement

expr :: MonadParser m => m Expr
expr =   Apply <$> name <*> many expr
     <|> ValueOf <$> name
     <|> LiteralVal <$> literal

name :: MonadParser m => m Name
name = token test Nothing
  where test (L _ (Tok.Name t)) = Right $ Name t
        test other              = didntExpect other

literal :: MonadParser m => m Lit
literal = token test Nothing
  where test (L _ (Tok.Literal t)) = Right $ t
        test other                 = didntExpect other

didntExpect :: (ErrorComponent e) => t -> Either (Set (ErrorItem t), Set (ErrorItem t), Set e) a
didntExpect a = Left (Set.singleton (Tokens $ a :| []), Set.empty, Set.empty)

reserved :: MonadParser m => Text -> m ()
reserved word = token test Nothing
  where test (L _ (Tok.ReservedWord w)) | word == w = Right ()
        test other = didntExpect other

symbol :: MonadParser m => Text -> m ()
symbol sym = token test Nothing
  where test (L _ (Tok.Symbol s)) | sym == s = Right ()
        test other = didntExpect other

-- Prelude

tuple :: Applicative f => f a -> f b -> f (a,b)
tuple = liftA2 (,)
