import Data.Text (Text, pack)
import Test.Tasty
import Test.Tasty.HUnit

import Eitri.Compiler

main :: IO ()
main = defaultMain $
          testGroup "Tests" $ uncurry tcase <$> [("simple fn",
                                                    "def foo() = { val a : Int = b }"),
                                                 ("eval expr",
                                                    "a"),
                                                 ("simple fn expr",
                                                    "def foo() = { a }"),
                                                 ("hello world",
                                                    helloWorld),
                                                 ("multiline",
                                                    multiLine)]

data TestResult = Success
                | ErrMsg String
                deriving (Eq)

instance Show TestResult where
  show Success = "Success"
  show (ErrMsg s) = s

tcase :: String -> Text -> TestTree
tcase nm = testCase nm . assertSucceeds --assertBool "Parses" . parseSucceeds

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left  _) = False

assertSucceeds :: Text -> Assertion
assertSucceeds = assertEqual "" Success . parseSucceeds'

parseSucceeds :: Text -> Bool
parseSucceeds = isRight . compile

parseSucceeds' :: Text -> TestResult
parseSucceeds' = fromLeft . compile
  where fromLeft (Left  s) = ErrMsg ('\n' : s)
        fromLeft (Right _) = Success

simpleFn :: Text
simpleFn = "def foo() = { val a : Int = b }"

helloWorld :: Text
helloWorld = "def helloWorld() = { print \"Hello World\"  }"

multiLine :: Text
multiLine = "def foo() = {\n val a : Int = 0\n val b : Int = 1\n add a b\n }"
