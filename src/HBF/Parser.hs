module HBF.Parser
  (
    parseProgram
  , Text.Parsec.ParseError
  )

where

import HBF.Types

import Text.Parsec (many1, many, between, ParseError, runP)
import Control.Applicative ((<|>))

import Text.Parsec.ByteString.Lazy
  ( Parser )

import Text.Parsec.Char
  ( char, noneOf, oneOf )

import qualified Data.ByteString.Lazy as BS

program :: Parser Program
program =
  many1 operation

operation :: Parser Op
operation = many garbage *> (simpleOp <|> loopOp) <* many garbage

simpleTokens :: String
simpleTokens = "><+-.,"

tokens :: String
tokens = "[]" ++ simpleTokens

garbage :: Parser Char
garbage = noneOf tokens

simpleOp :: Parser Op
simpleOp = build <$> oneOf simpleTokens
  where build '>' = MRight
        build '<' = MLeft
        build '+' = Inc
        build '-' = Dec
        build '.' = Out
        build ',' = In
        build _   = error "Unknown character"

loopOp :: Parser Op
loopOp = Loop <$> between (char '[') (char ']') program

-- | Parse program stream. Returns an error or the parsed 'Program'
parseProgram :: BS.ByteString -> Either ParseError Program
parseProgram = runP program () ""
