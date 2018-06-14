module CompilerTest where

import           Control.Monad.Trans.State (execStateT)
import qualified Data.Text.Lazy.IO         as TIO
import           Hedgehog
import qualified Hedgehog                  as H
import           Hedgehog.Checkers         as HC
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
          , Loop [MRight 1, MRight 2, Clear, Clear]
          , Loop [MRight 1, MRight (-1)] -- should eliminate this whole loop
          ]
      ]
    expected =
      [ Inc 1
      , MRight 2
      , Inc 1
      , In 3
      , Out 7
      , Loop [Inc 3, Loop [MRight 3, Clear]]
      ]

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

fullyFused :: Program o -> Bool
fullyFused p = all (uncurry fused) (zip ops (tail ops))
  where
    ops = instructions p
    fused (Inc _) (Inc _)       = False
    fused (MRight _) (MRight _) = False
    fused (In _) (In _)         = False
    fused (Out _) (Out _)       = False
    fused Clear Clear           = False
    fused (Loop inner) _        = fullyFused $ Program inner
    fused _ (Loop inner)        = fullyFused $ Program inner
    fused _ _                   = True

unit_fullyFusedTest :: Assertion
unit_fullyFusedTest =
  not (fullyFused $ Program [Inc 1, Inc 4]) &&
  not (fullyFused $ Program [Inc 1, Loop [Inc 1, Loop [MRight 1, MRight 2]]]) &&
  fullyFused (Program [Inc 1, MRight 2]) @=? True

hprop_fusionDoesntLeaveAnythingToBeFused :: Property
hprop_fusionDoesntLeaveAnythingToBeFused =
  property $ do
    program <- forAll programGen
    let optimized = fusionOpt . toIR $ program
    H.assert $ all noNoOp (flattened optimized) && fullyFused optimized
  where
    noNoOp (Inc n)    = n /= 0
    noNoOp (MRight n) = n /= 0
    noNoOp (In n)     = n /= 0
    noNoOp (Out n)    = n /= 0
    noNoOp Clear      = True
    noNoOp (Mul _ _) = True
    noNoOp (Loop _)   = error "noNoOp: unexpected operation"

hprop_FusedProgramHasValidMonoid :: Property
hprop_FusedProgramHasValidMonoid = property $ HC.monoid programGen

unit_clearOptimization :: Assertion
unit_clearOptimization = clearOpt (Program p) @?= Program expected
  where
    p =
      [ Inc 2
      , Inc (-1)
      , Loop [Inc (-1)]
      , Loop
          [ Inc 1
          , Loop [Inc (-1)]
          , Loop [Loop [Inc (-1)], MRight 3, Loop [Inc (-1)]]
          ]
      ]
    expected =
      [ Inc 2
      , Inc (-1)
      , Clear
      , Loop [Inc 1, Clear, Loop [Clear, MRight 3, Clear]]
      ]

unit_mulOptimization :: Assertion
unit_mulOptimization = mulOpt (Program p) @?= Program expected
  where
    p =
      [ Inc 2
      , Inc (-1)
      , makeMul [(1,2)]
      , Loop [Inc (-1), makeMul [(2, 4), (4,5)], Inc 1]
      , Loop [Inc (-1), MRight 1, Inc 1, MRight (-1), {- this extra part breaks the multiplication loop -} Inc 1] -- this tests missing eof
      ]
    expected =
      [ Inc 2
      , Inc (-1)
      , Mul 1 2, Clear
      , Loop [Inc (-1), Mul 2 4, Mul 6 5, Clear, Inc 1]
      , Loop [Inc (-1), MRight 1, Inc 1, MRight (-1), Inc 1]
      ]
