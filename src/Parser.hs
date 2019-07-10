{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Parser (runSub) where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           UnliftIO.Exception
import qualified Data.Text            as T
import           Data.Void
import           Control.Monad.State
import           Process
import           Control.Lens hiding (noneOf)

data S = S { _contents :: T.Text, _commands :: [T.Text] }
makeLenses ''S

type Parser = ParsecT Void T.Text (StateT S IO)

append :: T.Text -> Parser ()
append txt = contents <>= txt

block :: T.Text -> (T.Text -> Parser (Either T.Text T.Text)) -> Parser ()
block blockLabel process = do
    string ("```" <> blockLabel <> "\n") >>= append
    pos <- sourcePosPretty <$> getSourcePos
    content <- T.pack <$> manyTill anySingle (string "```")
    results <- process content
    case results of
        Left e ->
            throwString ("Error at " <> pos <> ":\n\n" <> T.unpack content <> "\n" <> T.unpack e )
        Right result -> append result
    append "```"

replBlock :: Parser ()
replBlock = block "repl" go
  where
    go txt = do
        cmds <- use commands
        liftIO $ runGHCISession cmds txt

ghciDirective :: Parser ()
ghciDirective = do
    string "\n$>" >>= append
    directive <- T.pack <$> manyTill (noneOf ['\n']) (single '\n')
    commands <>= [directive]
    append directive
    append "\n"

pass :: Parser ()
pass = anySingle >>= append . T.singleton

scanner :: Parser ()
scanner = void $ many (ghciDirective <|> replBlock <|> pass)

runSub :: String -> T.Text -> IO T.Text
runSub filename inp = _contents <$> execStateT (runParserT scanner filename inp) (S mempty mempty)
