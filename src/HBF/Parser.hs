{-|
Description : Brainfuck parsing
Copyright   : (c) Sebastian Galkin, 2018
License     : GPL-3

Parsing 'Text' into 'Program' 'Unparsed'
-}

module HBF.Parser
  ( module HBF.Parser
  -- * Reexport from "Text.Parsec"
  , Text.Parsec.ParseError
  ) where

import           Control.Applicative   ((<|>))
import           Data.Text.Lazy        (Text)
import           Text.Parsec           (ParseError, between, eof, many, many1,
                                        runP)
import           Text.Parsec.Char      (char, noneOf, oneOf)
import           Text.Parsec.Text.Lazy (Parser)

import           HBF.Types

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Either
-- >>> let parse :: Parser a -> Text -> Either ParseError a; parse p text = runP p () "" text

-- | Parser for a full 'Program'.
--
-- >>> isRight $ parse program "  +[->>+  +[<] ##garbage## ],.[-]  can ignore garbage"
-- True
program :: Parser (Program Unoptimized)
program = Program <$> many1 operation

-- | Parser for an 'Op', ignoring unknown characters.
--
-- >>> parse operation "  +///"
-- Right (Inc 1 0)
--
-- >>> parse operation "fooo  [+>]  baaar  "
-- Right (Loop [Inc 1 0,Move 1])
operation :: Parser Op
operation = many garbage *> (simpleOp <|> loopOp) <* many garbage

-- | The characters allowed in a Brainfuck program except for the loop characters @[@ and @]@.
bfSimpleTokens :: String
bfSimpleTokens = "><+-.,"

-- | The characters allowed in a Brainfuck program.
bfTokens :: String
bfTokens = "[]" ++ bfSimpleTokens

-- | Parser for unknown characters
--
-- >>> parse garbage "this is @#! garbage"
-- Right 't'
--
-- >>> isLeft $ parse garbage "+"
-- True
garbage :: Parser Char
garbage = noneOf bfTokens

-- | Parser for simple operations (not loops).
--
-- >>> parse simpleOp ">"
-- Right (Move 1)
--
-- >>> parse simpleOp "."
-- Right (Out 1 0)
simpleOp :: Parser Op
simpleOp = build <$> oneOf bfSimpleTokens
  where
    build '>' = Move 1
    build '<' = Move (-1)
    build '+' = Inc 1 0
    build '-' = Inc (-1) 0
    build '.' = Out 1 0
    build ',' = In 1 0
    build _   = error "Unknown character"

-- | Parser for loops.
--
-- >>> parse loopOp "[+-]"
-- Right (Loop [Inc 1 0,Inc (-1) 0])
loopOp :: Parser Op
loopOp = Loop . instructions <$> between (char '[') (char ']') program

-- | Parse program stream. Returns an error or the parsed 'Program'
parseProgram :: Text -> Either ParseError (Program Unoptimized)
parseProgram = runP (program <* eof) () ""
