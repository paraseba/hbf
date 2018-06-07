module HBF.Parser
  (
    parseProgram
  , bfSimpleTokens
  , bfTokens
  , Text.Parsec.ParseError
  )

where

import HBF.Types

import Text.Parsec (many1, many, between, ParseError, runP, eof)
import Control.Applicative ((<|>))

import Text.Parsec.Text.Lazy
  ( Parser )

import Text.Parsec.Char
  ( char, noneOf, oneOf )

import Data.Text.Lazy (Text)

program :: Parser Program
program =
  many1 operation

operation :: Parser Op
operation = many garbage *> (simpleOp <|> loopOp) <* many garbage

bfSimpleTokens :: String
bfSimpleTokens = "><+-.,"

bfTokens :: String
bfTokens = "[]" ++ bfSimpleTokens

garbage :: Parser Char
garbage = noneOf bfTokens

simpleOp :: Parser Op
simpleOp = build <$> oneOf bfSimpleTokens
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
parseProgram :: Text -> Either ParseError Program
parseProgram = runP (program <* eof) () ""
