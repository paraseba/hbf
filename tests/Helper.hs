{-# LANGUAGE OverloadedStrings #-}

module Helper where

import           Data.Coerce         (coerce)
import           Data.Int            (Int8)
import           Data.Semigroup      ((<>))
import           Data.Text.Lazy      (Text, pack)
import qualified Data.Vector.Unboxed as Vector
import           Hedgehog            (Gen)
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range

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
    (Gen.recursive weights [basic] [(: []) . Loop . instructions <$> programGen])
  where
    basic :: Gen [Op]
    basic =
      Gen.frequency
        [ (100, pure [Inc 1 0])
        , (50, pure [Inc (-1) 0])
        , (80, pure [MRight (-1)])
        , (70, pure [MRight 1])
        , (5, pure [Out 1 0])
        , (2, pure [In 1 0])
        , (10, (\b -> b ++ b) <$> basic) --fusable
        , (3, pure [Loop [MRight (-1)]]) --scanL
        , (2, pure [Loop [MRight 1]]) --scanR
        , (1, pure [Loop [Inc (-1) 0]]) --clear loop
        , ( 1
          , pure
              [ Inc (-1) 0
              , MRight 1
              , Inc 1 0
              , Inc 1 0
              , MRight 1
              , MRight 1
              , Inc (-1) 0
              , MRight (-1)
              , MRight (-1)
              ]) --mul loop --fixme use makeMul
        ]
    weights [nonrec] = nonrec
    weights (nonrec:recursive:_) = Gen.frequency [(10, nonrec), (1, recursive)]
    weights [] = error "programGen: unexpected condition"

makeMul :: [(MulFactor, MemOffset)] -> Op
makeMul muls =
  Loop $
  (Inc (-1) 0 : concatMap mkMul muls) ++
  replicate (coerce $ sum $ map snd muls) (MRight (-1))
  where
    mkMul (MulFactor fact, MemOffset off) = replicate off (MRight 1) ++ replicate fact (Inc 1 0)

listTape :: Tape (Vector.Vector Int8) -> Tape [Int8]
listTape t = Tape {pointer = pointer t, memory = Vector.toList (memory t)}
