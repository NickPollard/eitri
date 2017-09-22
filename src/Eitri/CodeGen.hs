module Eitri.CodeGen where

import Data.Monoid (Monoid(..), (<>))
import Data.String (IsString(..))
import Data.Text (unpack, Text, pack)
import qualified Data.Text as Text

import Eitri.AST

newtype Cpp = Cpp { unCpp :: Text }

instance Monoid Cpp where
  (Cpp a) `mappend` (Cpp b) = Cpp (a <> b)
  mempty = Cpp mempty

instance Show Cpp where
  show (Cpp t) = unpack t

instance IsString Cpp where
  fromString = Cpp . pack

showT :: Show a => a -> Text
showT = pack . show

class GenCpp a where
  genCpp :: a -> Cpp

intercalate :: Cpp -> [Cpp] -> Cpp
intercalate sep ts = Cpp $ Text.intercalate (unCpp sep) (unCpp <$> ts)

instance GenCpp Statement where
  genCpp (Assign var expr) = genCpp var <> " = " <>  genCpp expr <> ";"
  genCpp (Initialize var expr) = genCpp var <> " = " <>  genCpp expr <> ";"
  genCpp (Eval expr) = genCpp expr <> ";"
  genCpp (Block b) = genBlock b
  genCpp (FunDef name args block) = auto <> genCpp name <> "(" <> genArgs args <> ")" <> " = " <> genBlock block

genBlock :: [Statement] -> Cpp
genBlock ss = "{\n" <> intercalate "\n" (fmap genCpp ss) <> "\n}"

genArgs :: [VarName :- Type] -> Cpp
genArgs args = intercalate ", " $ genArg <$> args
 where genArg (arg, typ) = genCpp typ <> " " <> genCpp arg

instance GenCpp SomeVarDef where
  genCpp (SomeVarDef (Var n t)) = genCpp t <> " " <> genCpp n
  genCpp (SomeVarDef (Val n t)) = const' $ genCpp t <> " " <> genCpp n

const' :: Cpp -> Cpp
const' c = "const " <> c

instance GenCpp Expr where
  genCpp (Apply name []) = genCpp name
  genCpp (Apply name args) = genCpp name <> "(" <> intercalate ", " (genCpp <$> args) <> ")"
  genCpp (ValueOf name) = genCpp name
  genCpp (LiteralVal lit) = genCpp lit

instance GenCpp Lit where
  genCpp (LitString t) = Cpp $ "\"" <> t <> "\""
  genCpp (LitInt i) = Cpp $ showT i
  genCpp (LitDouble i) = Cpp $ showT i

instance GenCpp Name where
  genCpp (Name nm) = Cpp nm

instance GenCpp Type where
  genCpp (Type (Name "Int"))    = "int"
  genCpp (Type (Name "Double")) = "double"
  genCpp (Type (Name "Float"))  = "float"
  genCpp (Type nm) = genCpp nm

auto :: Cpp
auto = Cpp "auto "
