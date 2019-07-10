{-# LANGUAGE OverloadedStrings #-}
module Parser (runSub) where

import Text.Megaparsec
import Text.Megaparsec.Char
import UnliftIO.Exception

import qualified Data.Text as T
import Data.Void
import Control.Monad.State
import Process

type Parser = ParsecT Void T.Text (StateT T.Text IO)

append :: T.Text -> Parser ()
append txt = modify (<> txt)

block :: T.Text -> (T.Text -> Parser (Either T.Text T.Text)) -> Parser ()
block blockLabel process = do
    string ("```" <> blockLabel <> "\n") >>= append
    pos <- sourcePosPretty <$> getSourcePos
    content <- T.pack <$> manyTill anySingle (string "```")
    results <- process content
    case results of
        Left e -> do
            throwString ("Error at " <> pos <> ":\n\n" <> T.unpack content <> "\n" <> T.unpack e )
        Right result -> append result
    append "```"

replBlock :: Parser ()
replBlock = block "repl" (liftIO . runGHCISession)

pass :: Parser ()
pass = anySingle >>= append . T.singleton

scanner :: Parser ()
scanner = void $ many (replBlock <|> pass)

runSub :: String -> T.Text -> IO T.Text
runSub filename inp = execStateT (runParserT scanner filename inp) mempty
