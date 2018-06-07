module Main
where

import           Data.Monoid  ((<>))
import qualified System.Exit  as Exit

import           HBF.Compiler (ParseError, compile, parse)

main :: IO ()
main = do
  compilerOpts <- parse
  res <- compile compilerOpts
  either errorOut successOut res

errorOut :: ParseError -> IO ()
errorOut err = do
  putStrLn "Error compiling code"
  Exit.die (show err)

successOut :: Int -> IO ()
successOut n =
  putStrLn $ "Compiled code: " <> show n <> " instructions."
