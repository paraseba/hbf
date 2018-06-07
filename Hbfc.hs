{-# LANGUAGE RecordWildCards #-}

module Main
where

import System.Environment (getArgs)
import HBF.Compiler (compile, ParseError)
import qualified System.Exit as Exit
import Data.Monoid ((<>))

data CompilerArgs = CompilerArgs
  { cargsSource :: FilePath
  , cargsOut :: FilePath
  }

parseArgs :: IO CompilerArgs
parseArgs = do
  (inp:out:_) <- getArgs
  return CompilerArgs
    {cargsSource = inp, cargsOut = out}

main :: IO ()
main = do
  CompilerArgs {..} <- parseArgs
  res <- compile cargsSource cargsOut
  either errorOut successOut res

errorOut :: ParseError -> IO ()
errorOut err = do
  putStrLn "Error compiling code"
  Exit.die (show err)

successOut :: Int -> IO ()
successOut n =
  putStrLn $ "Compiled code: " <> show n <> " instructions."
