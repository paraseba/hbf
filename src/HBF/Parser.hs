module HBF.Parser
  where

import HBF.Types

import Text.Parsec.Prim
  ( (<|>), runP, skipMany )

import Text.Parsec.Combinator
  ( between, sepEndBy, eof )

import Text.Parsec.Error
  ( ParseError )

import Text.Parsec.ByteString.Lazy
  ( Parser )

import Text.Parsec.Char
  ( space, spaces, char )

import qualified Data.ByteString.Lazy as BS

program :: Parser Program
program = do
  skipMany space
  sepEndBy operation spaces

fullProgram :: Parser Program
fullProgram = program <* eof

operation :: Parser Op
operation = simpleOp <|> loop

simpleChar :: Parser Char
simpleChar = char '>' <|>
             char '<' <|>
             char '+' <|>
             char '-' <|>
             char '.' <|>
             char ','

simpleOp :: Parser Op
simpleOp = fmap build simpleChar
  where build '>' = MRight
        build '<' = MLeft
        build '+' = Inc
        build '-' = Dec
        build '.' = Out
        build ',' = In
        build _   = error "Unknown character"

loop :: Parser Op
loop = Loop <$> between (char '[') (char ']') program

-- | Parse program stream. Returns an error or the parsed 'Program'
parseProgram :: BS.ByteString -> Either ParseError Program
parseProgram = runP fullProgram () ""
