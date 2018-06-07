module HBF.Compiler
  (
    HBF.Parser.ParseError
  , module HBF.Compiler
  )
where

import HBF.Types
import HBF.Parser (parseProgram, ParseError)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Binary as B

compileP :: Program -> ByteString
compileP = B.encode

compilePToFile :: Program -> FilePath -> IO ()
compilePToFile = flip B.encodeFile

compile :: FilePath -> FilePath -> IO (Either ParseError Int)
compile inp out = do
  program <- parseProgram <$> BS.readFile inp
  either (return . Left) (\p -> compilePToFile p out >> return (Right $ length $ p)) program

load :: ByteString -> Program
load = B.decode

loadFile :: FilePath -> IO Program
loadFile = B.decodeFile
