import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Eitri.Compiler

main :: IO ()
main = defaultMain $
          testGroup "Tests" $ uncurry tcase <$> [("simple fn", "def foo() = { val a : Int = b }"),
                                                 ("eval expr", "a"),
                                                 ("simple fn expr", "def foo() = { a }"),
                                                 ("hello world", helloWorld)]

tcase :: String -> Text -> TestTree
tcase nm = testCase nm . assertBool "Parses" . parseSucceeds

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left  _) = False

parseSucceeds :: Text -> Bool
parseSucceeds = isRight . compile

simpleFn :: Text
simpleFn = "def foo() = { val a : Int = b }"

helloWorld :: Text
helloWorld = "def helloWorld() = { print \"Hello World\"  }"
