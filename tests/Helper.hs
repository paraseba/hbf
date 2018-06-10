{-# LANGUAGE OverloadedStrings #-}

module Helper where

import           Data.Semigroup ((<>))
import           Data.Text.Lazy (Text, pack)
import           Hedgehog       (Gen)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import           HBF.Parser
import           HBF.Types

basicOpCharGen :: Gen Char
basicOpCharGen = Gen.element bfSimpleTokens

commentCodeGen :: Gen Char
commentCodeGen = Gen.filter (not . flip elem bfTokens) Gen.unicode

basicCodeGen :: Gen Text
basicCodeGen = pack <$> Gen.filter hasCode strings
  where
    chars = Gen.frequency [(5, basicOpCharGen), (2, commentCodeGen)]
    strings = Gen.list (Range.linear 1 100) chars
    hasCode :: String -> Bool
    hasCode = any (`elem` bfSimpleTokens)

loopGen :: Gen Text
loopGen = fmap (\code -> "[" <> code <> "]") codeGen

codeGen :: Gen Text
codeGen = (<>) <$> block <*> block
  where
    block = Gen.frequency [(10, basicCodeGen), (1, loopGen)]

programGen :: Gen UnoptimizedProgram
programGen =
  Program <$>
  Gen.list
    (Range.linear 0 120)
    (Gen.recursive weights [basic] [Loop . instructions <$> programGen])
  where
    basic :: Gen BasicOp
    basic = Gen.element [Inc, Dec, MLeft, MRight, In, Out]
    weights [nonrec] = nonrec
    weights (nonrec:recursive:_) = Gen.frequency [(10, nonrec), (1, recursive)]
    weights [] = error "programGen: unexpected condition"
