{-# LANGUAGE OverloadedStrings #-}
module Process (runGHCISession) where

import UnliftIO.Process
import Data.Foldable
import System.IO
import System.Timeout
import Data.Traversable
import Control.Applicative
import Control.Monad
import Data.Maybe

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

runGHCISession :: T.Text -> IO T.Text
runGHCISession txt = do
    (stdInReadEnd, stdInWriteEnd) <- createPipe
    (stdOutReadEnd, stdOutWriteEnd) <- createPipe
    traverse_ (flip hSetBuffering LineBuffering)
              [stdInReadEnd, stdInWriteEnd, stdOutReadEnd, stdOutWriteEnd]
    let processDef = (proc "stack" ["exec", "ghci", "--", "-ignore-dot-ghci"])
            { std_in  = UseHandle stdInReadEnd
            , std_out = UseHandle stdOutWriteEnd
            }
    _ <- createProcess_ "err" processDef
    hPutStrLn stdInWriteEnd ":set prompt \">>>\""
    hFlush stdOutWriteEnd
    _ <- hGetLine stdOutReadEnd
    _ <- collectWithTimeout 1000 stdOutReadEnd
    results <- for (inputLines txt) $ \ln -> do
        hPutStrLn  stdInWriteEnd (T.unpack ln)
        hFlush stdInWriteEnd
        hFlush stdOutWriteEnd

        results <- collectWithTimeout 200 stdOutReadEnd
        return (ln <> "\n" <> T.pack results)
    let results' = (\t -> fromMaybe t (T.stripSuffix ">>>" t)) . T.strip $ ">>>" <> T.concat results
    TIO.putStr results'
    return results'

inputLines :: T.Text -> [T.Text]
inputLines = catMaybes .  fmap (T.stripPrefix ">>>") . T.lines

collectWithTimeout :: Int -> Handle -> IO String
collectWithTimeout timeoutMillis handle = do
    ready <- hWaitForInput handle timeoutMillis
    if ready then do
                c <- hGetChar handle
                (c :) <$> collectWithTimeout timeoutMillis handle
             else return []

txt :: T.Text
txt = ">>> let x = 5\n>>> x + 10\n15\n>>> x + 20\n25"
