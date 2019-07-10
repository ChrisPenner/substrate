{-# LANGUAGE OverloadedStrings #-}
module Process (runGHCISession) where

import UnliftIO.Process
import Data.Foldable
import System.IO
import Data.Traversable
import Data.Maybe
import System.Exit
import UnliftIO.Exception

import qualified Data.Text as T

runGHCISession :: T.Text -> IO (Either T.Text T.Text)
runGHCISession txt = do
    (stdInReadEnd, stdInWriteEnd) <- createPipe
    (stdOutReadEnd, stdOutWriteEnd) <- createPipe
    traverse_ (flip hSetBuffering LineBuffering)
              [stdInReadEnd, stdInWriteEnd, stdOutReadEnd, stdOutWriteEnd]
    let processDef = (proc "stack" ["exec", "ghci", "--", "-ignore-dot-ghci"])
            { std_in  = UseHandle stdInReadEnd
            , std_out = UseHandle stdOutWriteEnd
            , std_err = CreatePipe
            }
    (_, _, Just stdErrReadEnd, procHandle) <- createProcess_ "err" processDef
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

    terminateProcess procHandle
    exitCode <- getProcessExitCode procHandle
    errs <- T.pack <$> hGetContents stdErrReadEnd
    case (exitCode, errs) of
        (Just failure@(ExitFailure {}), e) -> return $ Left (T.pack (show failure <> "\n") <> e)
        (_, e) | T.null e -> do
            let results' = (\t -> fromMaybe t (T.stripSuffix ">>>" t)) . T.strip $ ">>>" <> T.concat results
            return $ Right results'
        (_, e) -> return $ Left e

inputLines :: T.Text -> [T.Text]
inputLines = mapMaybe (T.stripPrefix ">>>") . T.lines

collectWithTimeout :: Int -> Handle -> IO String
collectWithTimeout timeoutMillis h = do
    ready <- hWaitForInput h timeoutMillis
    if ready then do
                c <- hGetChar h
                (c :) <$> collectWithTimeout timeoutMillis h
             else return []

sample :: T.Text
sample = ">>> let x = 5\n>>> x + 10\n15\n>>> x + 20\n25"
