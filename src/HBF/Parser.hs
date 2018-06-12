module HBF.Parser
  ( parseProgram
  , bfSimpleTokens
  , bfTokens
  , Text.Parsec.ParseError
  ) where

import           Control.Applicative   ((<|>))
import           Data.Text.Lazy        (Text)
import           Text.Parsec           (ParseError, between, eof, many, many1,
                                        runP)
import           Text.Parsec.Char      (char, noneOf, oneOf)
import           Text.Parsec.Text.Lazy (Parser)

import           HBF.Types

program :: Parser (Program Unoptimized)
program = Program <$> many1 operation

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
  where
    build '>' = MRight 1
    build '<' = MRight (-1)
    build '+' = Inc 1
    build '-' = Inc (-1)
    build '.' = Out 1
    build ',' = In 1
    build _   = error "Unknown character"

loopOp :: Parser Op
loopOp = Loop . instructions <$> between (char '[') (char ']') program

-- | Parse program stream. Returns an error or the parsed 'Program'
parseProgram :: Text -> Either ParseError (Program Unoptimized)
parseProgram = runP (program <* eof) () ""
