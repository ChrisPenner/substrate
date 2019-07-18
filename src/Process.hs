{-# LANGUAGE OverloadedStrings #-}
module Process (runGHCISession) where

import UnliftIO.Process
import Data.Foldable
import System.IO
import Data.Traversable
import Data.Maybe
import System.Exit

import qualified Data.Text as T

initialTimeoutMillis :: Int
initialTimeoutMillis = 1000
standardTimeoutMillis :: Int
standardTimeoutMillis = 200

runGHCISession :: [T.Text] -> T.Text -> IO (Either T.Text T.Text)
runGHCISession preamble txt = do
    (stdInReadEnd, stdInWriteEnd) <- createPipe
    (stdOutReadEnd, stdOutWriteEnd) <- createPipe
    traverse_ (flip hSetBuffering LineBuffering)
              [stdInReadEnd, stdInWriteEnd, stdOutReadEnd, stdOutWriteEnd]
    let processDef = (proc "stack" ["repl", "--ghci-options", "-ignore-dot-ghci"])
            { std_in  = UseHandle stdInReadEnd
            , std_out = UseHandle stdOutWriteEnd
            , std_err = CreatePipe
            }
    (_, _, Just stdErrReadEnd, procHandle) <- createProcess_ "err" processDef
    for_ ( ":set prompt \">>>\"" : preamble) $ hPutStrLn stdInWriteEnd . T.unpack
    -- hPutStrLn stdInWriteEnd ":set +m"
    hFlush stdOutWriteEnd
    _ <- hGetLine stdOutReadEnd
    _ <- collectWithTimeout initialTimeoutMillis stdOutReadEnd
    results <- for (inputLines txt) $ \ln -> do
        hPutStrLn  stdInWriteEnd (T.unpack ln)
        hFlush stdInWriteEnd
        hFlush stdOutWriteEnd

        results <- collectWithTimeout standardTimeoutMillis stdOutReadEnd
        return (ln <> "\n" <> T.pack results)

    terminateProcess procHandle
    exitCode <- getProcessExitCode procHandle
    errs <- T.pack <$> hGetContents stdErrReadEnd
    case (exitCode, errs) of
        (Just failure@(ExitFailure {}), e) -> return $ Left (T.pack (show failure <> "\n") <> e)
        (_, e) -> do
            let results' = (\t -> fromMaybe t (T.stripSuffix ">>>" t)) . T.strip $ ">>>" <> T.concat results
            hPutStrLn stderr (T.unpack e)
            return $ Right results'

inputLines :: T.Text -> [T.Text]
inputLines = mapMaybe (T.stripPrefix ">>>") . T.lines

collectWithTimeout :: Int -> Handle -> IO String
collectWithTimeout timeoutMillis h = do
    ready <- hWaitForInput h timeoutMillis
    if ready then do
                c <- hGetChar h
                (c :) <$> collectWithTimeout timeoutMillis h
             else return []
