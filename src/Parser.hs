{-# LANGUAGE OverloadedStrings #-}
module Parser (runScanner) where

import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void
import Control.Monad.State
import Data.Char
import Process

type Parser = ParsecT Void T.Text (StateT T.Text IO)

append :: T.Text -> Parser ()
append txt = modify (<> txt)

block :: T.Text -> (T.Text -> Parser T.Text) -> Parser ()
block blockLabel process = do
    string "```repl\n" >>= append
    content <- T.pack <$> manyTill anySingle (string "```")
    process content >>= append
    append "```"

replBlock :: Parser ()
replBlock = block "repl" (liftIO . runGHCISession)

pass :: Parser ()
pass = anySingle >>= append . T.singleton

scanner :: Parser ()
scanner = void $  many (replBlock <|> pass)

runScanner :: T.Text -> IO T.Text
runScanner inp = execStateT (runParserT scanner "" inp) mempty

test :: IO ()
test = do
    testFile <- TIO.readFile "test.md"
    result <- execStateT (runParserT scanner "" testFile) mempty
    TIO.putStrLn result
