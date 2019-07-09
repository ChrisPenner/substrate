{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
import Process
import Parser
import qualified Data.Text as T
import Test.Hspec
import Text.RawString.QQ

inp :: T.Text
inp = [r|# title

some text

```repl
>>> let x = 5
>>> x
output
>>> x * 10
```
|]

expected :: T.Text
expected = [r|# title

some text

```repl
>>> let x = 5
>>> x
5
>>> x * 10
50
```
|]

main :: IO ()
main = hspec $ do
    describe "scanner" $ do
        it "should run ghci sessions" $
            runScanner inp `shouldReturn` expected
