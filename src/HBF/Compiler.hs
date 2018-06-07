{-# LANGUAGE RecordWildCards #-}

module HBF.Compiler
  (
    BFP.ParseError
  , module HBF.Compiler
  )
where

import HBF.Types
import qualified HBF.Parser as BFP

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Binary as B
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)
import System.FilePath ((-<.>))

compileP :: Program -> ByteString
compileP = B.encode

compilePToFile :: Program -> FilePath -> IO ()
compilePToFile = flip B.encodeFile

compile :: CompilerOptions -> IO (Either BFP.ParseError Int)
compile CompilerOptions{..} = do
  program <- BFP.parseProgram <$> BS.readFile cOptsSource
  either (return . Left) (\p -> compilePToFile p outPath >> return (Right $ length $ p)) program
  where
    outPath = fromMaybe (cOptsSource -<.> "bfc") cOptsOut

load :: ByteString -> Program
load = B.decode

loadFile :: FilePath -> IO Program
loadFile = B.decodeFile

data CompilerOptions = CompilerOptions
  { cOptsOut :: Maybe FilePath
  , cOptsFusionOptimization :: Bool
  , cOptsSource :: FilePath
  }

optionsP :: Parser CompilerOptions
optionsP = CompilerOptions
  <$>
    optional (option str
    (long "output"
    <> short 'o'
    <> metavar "OUT"
    <> help "Compiled output path"))

  <*> flag True False
    ( long "disable-fusion-optimization"
    <> help "Disable fusion optimization (turn multiple + or > in a single operation)")

  <*> argument str
    (metavar "SRC"
    <> help "Input source code file")

options :: ParserInfo CompilerOptions
options = info (optionsP <**> helper)
  (fullDesc
  <> progDesc "Compile Brainfuck code in SRC file"
  <> header "An optimizing Brainfuck compiler and evaluator")

parse :: IO CompilerOptions
parse = execParser options
