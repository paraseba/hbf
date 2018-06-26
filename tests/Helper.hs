{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

-- needed for smallcheck
module Helper where

import           Control.Monad.Trans.State (runStateT)
import           Data.Coerce               (coerce)
import           Data.Int                  (Int8)
import           Data.Semigroup            (Semigroup, (<>))
import           Data.Text.Lazy            (Text, pack)
import qualified Data.Text.Lazy.IO         as TIO
import qualified Data.Vector.Unboxed       as Vector
import           Hedgehog                  (Gen, PropertyT, forAll, (===))
import qualified Hedgehog.Checkers         as HC
import qualified Hedgehog.Gen              as Gen
import qualified Hedgehog.Range            as Range
import           Test.SmallCheck.Series

import           HBF.Compiler              (CompilerOptions (..),
                                            defaultCompilerOptions,
                                            inMemoryCompile)
import           HBF.Eval                  (MachineType, eval)
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
        , (80, pure [Move (-1)])
        , (70, pure [Move 1])
        , (5, pure [Out 1 0])
        , (2, pure [In 1 0])
        , (10, (\b -> b ++ b) <$> basic) --fusable
        , (3, pure [Loop [Move (-1)]]) --scanL
        , (2, pure [Loop [Move 1]]) --scanR
        , (1, pure [Loop [Inc (-1) 0]]) --clear loop
        , ( 1
          , pure
              [ Inc (-1) 0
              , Move 1
              , Inc 1 0
              , Inc 1 0
              , Move 1
              , Move 1
              , Inc (-1) 0
              , Move (-1)
              , Move (-1)
              ]) --mul loop --fixme use makeMul
        ]
    weights [nonrec] = nonrec
    weights (nonrec:recursive:_) = Gen.frequency [(10, nonrec), (1, recursive)]
    weights [] = error "programGen: unexpected condition"

makeMul :: [(MulFactor, MemOffset)] -> Op
makeMul muls =
  Loop $
  (Inc (-1) 0 : concatMap mkMul muls) ++
  replicate (coerce $ sum $ map snd muls) (Move (-1))
  where
    mkMul (MulFactor fact, MemOffset off) =
      replicate off (Move 1) ++ replicate fact (Inc 1 0)

listMachine :: Machine (Vector.Vector Int8) -> Machine [Int8]
listMachine t = t {memory = Vector.toList (memory t)}

newtype CompFlags =
  CompFlags CompilerOptions
  deriving (Show)

instance Monad m => Serial m CompFlags where
  series =
    cons5
      (\a b c d e ->
         CompFlags
           CompilerOptions
             { cOptsOut = Nothing
             , cOptsFusionOptimization = a
             , cOptsClearLoopOptimization = b
             , cOptsMulOptimization = c
             , cOptsScanOptimization = d
             , cOptsOffsetInstructionsOptimization = e
             , cOptsVerbose = False
             , cOptsSource = ""
             })

cons5 ::
     (Serial m a5, Serial m a4, Serial m a3, Serial m a2, Serial m a1)
  => (a1 -> a2 -> a3 -> a4 -> a5 -> a6)
  -> Series m a6
cons5 f = decDepth $ f <$> series <~> series <~> series <~> series <~> series

execProgram :: Program Optimized -> MockIO -> IO (MachineType, MockIO)
execProgram p = runStateT (eval p)

execProgramS :: Program Optimized -> String -> IO (MachineType, MockIO)
execProgramS p input = runStateT (eval p) (mkMockIOS input)

execCodeMock :: Text -> String -> IO (MachineType, MockIO)
execCodeMock code input = execProgram p (mkMockIOS input)
  where
    (Right (p, _)) = inMemoryCompile defaultCompilerOptions code

execCode :: Text -> String -> IO (MachineType, String)
execCode code input = fmap mockOutputS <$> execCodeMock code input

execFile :: FilePath -> String -> IO (MachineType, String)
execFile p input = TIO.readFile p >>= \code -> execCode code input

-- | We redefine the monoid checker from hedgehog-checkers because it does expensive unnecessary extra work
monoid :: (Monoid a, Semigroup a, Eq a, Show a) => Gen a -> PropertyT IO ()
monoid gen = do
  HC.semigroup gen
  HC.identity mappend mempty gen
  monoidSemigroupSame
  where
    monoidSemigroupSame = do
      a <- forAll gen
      b <- forAll gen
      mappend a b === a <> b
