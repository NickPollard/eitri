{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Eitri.AST where

import Data.Monoid ((<>))
import Data.Text (Text, unpack)

data Lit = LitString Text
         | LitInt Int
         | LitDouble Double
        deriving (Eq, Ord)

instance Show Lit where
  show (LitString t) = "\"" <> unpack t <> "\""
  show (LitInt i) = show i
  show (LitDouble d) = show d

data Name = Name Text
  deriving (Show)
type Block = [Statement]
type VarName = Name
type FnName = Name

type Type = Name

{-
data Type = String
  | Int
  |  Double
  | Boolean
          -}

data Expr = Apply FnName [Expr] -- foo(a, b)
          | ValueOf VarName     -- a
          | LiteralVal Lit      -- "hello"
          deriving (Show)

data Mutability = Immutable
                | Mutable
                deriving (Show)

data VarDef (m :: Mutability) where
  Var :: Name -> Type -> VarDef 'Mutable
  Val :: Name -> Type -> VarDef 'Immutable

instance Show (VarDef m) where
  show (Var n t) = show n <> show t
  show (Val n t) = show n <> show t

data SomeVarDef where
  SomeVarDef :: VarDef m -> SomeVarDef

instance Show SomeVarDef where
  show (SomeVarDef v@(Var _ _)) = show v
  show (SomeVarDef v@(Val _ _)) = show v

data Statement = Assign VarName Expr
               | Initialize SomeVarDef Expr
               | Eval Expr
               | Block [Statement]
               | FunDef Name ArgDefs Block
               deriving (Show)

type (:-) a b = (a,b)

type ArgDefs = [VarName :- Type]

{-
  We would like to parse some simple function definitions:

  def foo() = 2

  def id(a) = a

  def bar() =
    val a = 2
    foo(2)
   -}
