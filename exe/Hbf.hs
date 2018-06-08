{-# LANGUAGE RecordWildCards #-}

module Main
where

import System.Environment (getArgs)
import HBF.Eval (eval)
import HBF.Compiler (loadFile)
import System.IO (hFlush, stdout)

data VMArgs = VMArgs
  { vmargsIR :: FilePath
  }

parseArgs :: IO VMArgs
parseArgs = do
  (inp:_) <- getArgs
  return VMArgs
    {vmargsIR = inp}

main :: IO ()
main = do
  VMArgs {..} <- parseArgs
  program <- loadFile vmargsIR
  _ <- eval program
  hFlush stdout
  return ()
