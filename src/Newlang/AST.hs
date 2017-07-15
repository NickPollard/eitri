{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Newlang.AST where

import Data.Text (Text)

data Name = Name Text
type Block = [Statement]
type VarName = Name
type FnName = Name

data Type = String
          | Int
          | Double
          | Boolean

data Expr = Apply FnName [Expr] -- foo(a, b)
          | ValueOf VarName     -- a

data Mutability = Immutable
                | Mutable

data VarDef (m :: Mutability) where
  Var :: Name -> Type -> VarDef 'Mutable
  Val :: Name -> Type -> VarDef 'Immutable

data SomeVarDef where
  SomeVarDef :: VarDef m -> SomeVarDef

data Statement = Assign VarName Expr
               | Initialize SomeVarDef Expr
               | Eval Expr
               | Block [Statement]
               | FunDef Name ArgDefs Block

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
