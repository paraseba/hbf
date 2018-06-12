module CompilerTest where

import           Control.Monad.Trans.State (execStateT)
import           Data.Foldable             (toList)
import qualified Data.Text.Lazy.IO         as TIO
import           Hedgehog
import qualified Hedgehog                  as H
import           Test.HUnit

import           HBF.Compiler
import qualified HBF.Eval                  as E
import           HBF.Types
import           Helper

unit_fusionOptimization :: Assertion
unit_fusionOptimization = fusionOpt (Program p) @?= Program expected
  where
    p =
      [ Inc 2
      , Inc (-1)
      , MRight 3
      , MRight (-1)
      , Inc 1
      , In 1
      , In 2
      , Out 3
      , Out 4
      , Inc 2
      , Inc (-2) -- should eliminate this
      , Loop
          [ Inc 1
          , Inc 2
          , Loop [MRight 1, MRight 2]
          , Loop [MRight 1, MRight (-1)] -- should eliminate this whole loop
          ]
      ]
    expected =
      [Inc 1, MRight 2, Inc 1, In 3, Out 7, Loop [Inc 3, Loop [MRight 3]]]

unit_optimizationsDontChangeResults :: Assertion
unit_optimizationsDontChangeResults = do
  code <- TIO.readFile "tests/factor.bf"
  let (Right (optimized, _)) = inMemoryCompile defaultCompilerOptions code
  let (Right (unoptimized, _)) =
        inMemoryCompile noOptimizationCompilerOptions code
  a <- exec optimized
  b <- exec unoptimized
  a @?= b
  where
    exec program = execStateT (E.eval program) (mkMockIOS "25454\n")

fullyFused :: [Op] -> Bool
fullyFused ops = all (uncurry fused) (zip (toList ops) (tail (toList ops)))
  where
    fused (Inc _) (Inc _)       = False
    fused (MRight _) (MRight _) = False
    fused (In _) (In _)         = False
    fused (Out _) (Out _)       = False
    fused (Loop inner) _        = fullyFused inner
    fused _ (Loop inner)        = fullyFused inner
    fused _ _                   = True

unit_fullyFusedTest :: Assertion
unit_fullyFusedTest =
  not (fullyFused [Inc 1, Inc 4]) &&
  not (fullyFused [Inc 1, Loop [Inc 1, Loop [MRight 1, MRight 2]]]) &&
  fullyFused [Inc 1, MRight 2] @=? True

hprop_fusionDoesntLeaveAnythingToBeFused :: Property
hprop_fusionDoesntLeaveAnythingToBeFused =
  property $ do
    program <- forAll programGen
    let (Program optimized) = fusionOpt . toIR $ program
    H.assert $ all noNoOp optimized && fullyFused optimized
  where
    noNoOp (Inc n)    = n /= 0
    noNoOp (MRight n) = n /= 0
    noNoOp (In n)     = n /= 0
    noNoOp (Out n)    = n /= 0
    noNoOp (Loop ops) = all noNoOp ops
