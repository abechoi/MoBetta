module Main where

-- Abe Choi
-- CPSC-354-01

import System.Environment
import System.IO -- needed for file handling
import Control.Monad.Trans.State.Lazy
import Text.Megaparsec.Error


import MoBettaParser -- construct abstract syntax trees from source code
import MoBettaEngine -- construct computations from abstract syntax trees

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
