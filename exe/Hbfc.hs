module Hbfc where

import           Data.Monoid  ((<>))
import           System.Exit  (die)

import           HBF.Compiler (CompilationSummary, ParseError, compile, parse)

main :: IO ()
main = do
  compilerOpts <- parse
  res <- compile compilerOpts
  either errorOut successOut res

errorOut :: ParseError -> IO ()
errorOut err = do
  putStrLn "Error compiling code"
  die (show err)

successOut :: CompilationSummary -> IO ()
successOut s = putStrLn $ "Compiled code:\n" <> show s
