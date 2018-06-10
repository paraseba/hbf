{-# LANGUAGE OverloadedStrings #-}

module ParserTest where

import           Data.Either    (isLeft, isRight)
import           Data.Semigroup ((<>))
import           Data.Text.Lazy (Text, pack)
import           Hedgehog
import qualified Hedgehog       as H
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import           Test.HUnit
import qualified Test.HUnit     as HU

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
codeGen = (<>) <$> block <*>  block
  where block = Gen.frequency [(5, basicCodeGen), (1, loopGen)]


unit_parseBasicProgram :: Assertion
unit_parseBasicProgram =
  parseProgram "+><-[+,.[-]<<]" @?= Right (Program result)
  where result = [Inc,MRight,MLeft,Dec,
                  Loop [Inc,In,Out,
                        Loop [Dec],
                        MLeft,MLeft]]

unit_cantParseEmpty :: Assertion
unit_cantParseEmpty =
  HU.assert $ isLeft $ parseProgram ""

hprop_parseRandomPrograms :: Property
hprop_parseRandomPrograms = property $
  forAll codeGen >>=
    H.assert . isRight . parseProgram

hprop_cantParseUnbalancedLoop :: Property
hprop_cantParseUnbalancedLoop = property $ do
  p1 <- forAll $ Gen.frequency [(10,codeGen), (1, Gen.constant "")]
  p2 <- forAll $ Gen.frequency [(10,codeGen), (1, Gen.constant "")]
  braket <- forAll $ Gen.element ["[", "]"]
  H.assert $ isLeft $ parseProgram (p1 <> braket <> p2)
