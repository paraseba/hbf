module HBF.Parser
  (
    parseProgram
  , bfSimpleTokens
  , bfTokens
  , Text.Parsec.ParseError
  )

where


import           Control.Applicative   ((<|>))
import           Data.Text.Lazy        (Text)
import           Text.Parsec           (ParseError, between, eof, many, many1,
                                        runP)
import           Text.Parsec.Char      (char, noneOf, oneOf)
import           Text.Parsec.Text.Lazy (Parser)

import           HBF.Types

program :: Parser UnoptimizedProgram
program =
  Program <$> many1 operation

operation :: Parser BasicOp
operation = many garbage *> (simpleOp <|> loopOp) <* many garbage

bfSimpleTokens :: String
bfSimpleTokens = "><+-.,"

bfTokens :: String
bfTokens = "[]" ++ bfSimpleTokens

garbage :: Parser Char
garbage = noneOf bfTokens

simpleOp :: Parser BasicOp
simpleOp = build <$> oneOf bfSimpleTokens
  where build '>' = MRight
        build '<' = MLeft
        build '+' = Inc
        build '-' = Dec
        build '.' = Out
        build ',' = In
        build _   = error "Unknown character"

loopOp :: Parser BasicOp
loopOp = Loop . instructions <$> between (char '[') (char ']') program

-- | Parse program stream. Returns an error or the parsed 'Program'
parseProgram :: Text -> Either ParseError UnoptimizedProgram
parseProgram = runP (program <* eof) () ""
