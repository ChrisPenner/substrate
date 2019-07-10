module Main where

import System.Environment
import Parser
import qualified Data.Text.IO as TIO
import Data.Foldable

main :: IO ()
main = do
    filePaths <- getArgs
    for_ filePaths $ \fp -> do
        contents <- TIO.readFile fp
        results <- runSub contents
        TIO.writeFile fp results
