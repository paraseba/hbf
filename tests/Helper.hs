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

programGen :: Gen (Program Unoptimized)
programGen =
  Program . concat <$>
  Gen.list
    (Range.linear 0 120)
    (Gen.recursive weights [basic] [(:[]) . Loop . instructions <$> programGen])
  where
    basic :: Gen [Op]
    basic =
      Gen.frequency
        [ (100, pure  [Inc 1])
        , (50, pure  [ Inc (-1) ])
        , (80, pure  [ MRight (-1) ])
        , (70, pure  [ MRight 1 ])
        , (5, pure  [ Out 1 ])
        , (2, pure [ In 1 ])
        , (1, pure [ Loop [Inc (-1)] ]) --clear loop

        , (10, (\b -> b ++ b) <$> basic) --fusable
        ]
    weights [nonrec] = nonrec
    weights (nonrec:recursive:_) = Gen.frequency [(10, nonrec), (1, recursive)]
    weights [] = error "programGen: unexpected condition"
