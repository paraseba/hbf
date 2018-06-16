{-# LANGUAGE OverloadedStrings #-}

module ParserTest where

import           Data.Either    (isLeft, isRight)
import           Data.Semigroup ((<>))
import           Hedgehog
import qualified Hedgehog       as H
import qualified Hedgehog.Gen   as Gen
import           Test.HUnit
import qualified Test.HUnit     as HU

import           HBF.Parser
import           HBF.Types
import           Helper

unit_parseBasicProgram :: Assertion
unit_parseBasicProgram =
  parseProgram "+><-[+,.[-]<<]" @?= Right (Program result)
  where
    result =
      [ Inc 1 0
      , MRight 1
      , MRight (-1)
      , Inc (-1) 0
      , Loop
          [ Inc 1 0
          , In 1 0
          , Out 1 0
          , Loop [Inc (-1) 0]
          , MRight (-1)
          , MRight (-1)
          ]
      ]

unit_cantParseEmpty :: Assertion
unit_cantParseEmpty = HU.assert $ isLeft $ parseProgram ""

hprop_parseRandomPrograms :: Property
hprop_parseRandomPrograms =
  property $ forAll codeGen >>= H.assert . isRight . parseProgram

hprop_cantParseUnbalancedLoop :: Property
hprop_cantParseUnbalancedLoop =
  property $ do
    p1 <- forAll $ Gen.frequency [(10, codeGen), (1, Gen.constant "")]
    p2 <- forAll $ Gen.frequency [(10, codeGen), (1, Gen.constant "")]
    braket <- forAll $ Gen.element ["[", "]"]
    H.assert $ isLeft $ parseProgram (p1 <> braket <> p2)
