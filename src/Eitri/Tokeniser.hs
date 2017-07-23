module Eitri.Tokeniser (
  Tok(..),
  tokenise,
  TokenStream
) where

import Data.Bifunctor (second)
import qualified Data.List.NonEmpty as NE
import Data.Loc (L(..), Loc(..), Pos(..))
import qualified Data.Loc as Loc
import qualified Data.List as List
import Data.List (intercalate)
import Data.Text (Text, pack, unpack)
import Text.Megaparsec.Combinator
import Text.Megaparsec.Error
import Text.Megaparsec.Prim
import Text.Megaparsec.Pos (SourcePos(..))
import qualified Text.Megaparsec as MP
import Text.Megaparsec (string, many, some, alphaNumChar, space, (<|>), char, noneOf, digitChar)

import Eitri.AST (Lit(..))

data Tok = Name Text
         | ReservedWord Text
         | Symbol Text
         | Literal Lit
         deriving (Eq, Ord)

instance Show Tok where
  show (Name t) = unpack t
  show (ReservedWord t) = unpack t
  show (Symbol t) = unpack t
  show (Literal l) = show l


newtype TokenStream = TokenStream [L Tok] deriving Show

instance Stream TokenStream where
  type Token TokenStream = L Tok
  uncons (TokenStream l) = second TokenStream <$> List.uncons l
  updatePos _ _ _ (L (Loc beg end) _) = (toSrc beg, toSrc end)
  updatePos _ _ _ (L NoLoc _) = error "No Loc"

instance ShowToken (L Tok) where
  showTokens ts = intercalate ", " $ show <$> NE.toList ts

toSrc :: Pos -> SourcePos
toSrc (Pos f l c _) = SourcePos f (toPos l) (toPos c)

toPos :: Int -> MP.Pos
toPos = MP.unsafePos . fromIntegral

reservedWords :: [String]
reservedWords = "def"
              : "return"
              : "if"
              : "then"
              : "else"
              : "case"
              : "var"
              : "val"
              : []

symbol' :: [String]
symbol' = "("
        : ")"
        : "["
        : "]"
        : "}"
        : "{"
        : ":"
        : "="
        : []

type MonadParser m = MonadParsec Dec Text m

txt :: MonadParser m => String -> m Text
txt x = pack <$> string x

name' :: MonadParser m => m Text
name' = pack <$> some alphaNumChar

symbols :: MonadParser m => m Text
symbols = choice $ txt <$> symbol'

reserved :: MonadParser m => m Text
reserved = choice $ txt <$> reservedWords

litString :: MonadParser m => m Lit
litString = LitString . pack <$> (char '"' *> many (noneOf ['"']) <* char '"') -- TODO escaping

litInt :: MonadParser m => m Lit
litInt = LitInt . read <$> some digitChar

literal :: MonadParser m => m Lit
literal = litString <|> litInt

withSpaces :: MonadParser m => m a -> m a
withSpaces p = p <* space

someToken :: MonadParser m => m (L Tok)
someToken = withSpaces . withPos $ ReservedWord <$> reserved
                               <|> Symbol <$> symbols
                               <|> Name <$> name'
                               <|> Literal <$> literal

withPos :: MonadParser m => m a -> m (L a)
withPos p = mkL <$> getPos <*> p <*> getPos
  where mkL beg a end = L (Loc beg end) a
        getPos = sourceToLoc <$> getPosition
        sourceToLoc (MP.SourcePos f l c) = Loc.Pos f (fromIntegral $ MP.unPos l) (fromIntegral $ MP.unPos c) (fromIntegral $ MP.unPos c)

tokeniser :: MonadParser m => m TokenStream
tokeniser = TokenStream <$> many someToken

tokenise :: Text -> Either (ParseError (Token Text) Dec)  TokenStream
tokenise = parse (tokeniser <* eof) noFile
  where noFile = ""
