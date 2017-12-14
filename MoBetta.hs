module Main where

-- Abe Choi
-- CPSC-354-01

import System.Environment
import System.IO
import Control.Monad.Trans.State.Lazy
import Text.Megaparsec.Error


import MoBettaParser
import MoBettaEngine

main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    let fileName = head args
    putStrLn $ "MoBetta running file: " ++ fileName ++ " ...\n"
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let pr = mbparse fileName contents
    case pr of
      Right prog -> evalStateT (makeProgram prog) emptyEnv
      Left err  -> putStrLn $ parseErrorPretty' contents err
    hClose handle
