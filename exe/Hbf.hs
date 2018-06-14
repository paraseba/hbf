{-# LANGUAGE RecordWildCards #-}

module Hbf where

import           HBF.Compiler       (loadFile)
import           HBF.Eval           (eval)
import           System.Environment (getArgs)
import           System.IO          (hFlush, stdout)

newtype VMArgs = VMArgs
  { vmargsIR :: FilePath
  }

parseArgs :: IO VMArgs
parseArgs = do
  (inp:_) <- getArgs
  return VMArgs {vmargsIR = inp}

main :: IO ()
main = do
  VMArgs {..} <- parseArgs
  program <- loadFile vmargsIR
  _ <- eval program
  hFlush stdout
  return ()
