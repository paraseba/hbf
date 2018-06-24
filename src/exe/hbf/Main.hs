module Main where

import           HBF.Compiler (loadFile)
import           HBF.Eval     (VMOptions (..), evalWithIO, parse)
import           System.IO    (hFlush, stdout)

main :: IO ()
main = do
  vmOpts <- parse
  program <- loadFile (vmOptsProgramPath vmOpts)
  _ <- evalWithIO vmOpts program
  hFlush stdout
