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
      [ Inc 2 1
      , Inc (-1) 1
      , Move 3
      , Move (-1)
      , Inc 1 0
      , In 1 0
      , In 2 0
      , Out 3 0
      , Out 4 0
      , Inc 2 0
      , Inc (-2) 0 -- should eliminate this
      , Out 1 0 --should not fuse the next two, but should fuse this one with the previous Out
      , Out 1 1
      , Out 1 2
      , Loop
          [ Inc 1 0
          , Inc 2 0
          , Loop [Move 1, Move 2, Clear 0, Clear 0, Clear 1, Clear 2]
          , Loop [Move 1, Move (-1)] -- should eliminate this whole loop
          , Scan Up 0
          , Scan Up 0
          , Scan Down 3
          , Scan Down 3
          , Scan Up 0 -- shouldn't fuse the next two
          , Scan Up 3
          , Scan Down 0 -- shouldn't fuse the next two
          , Scan Down 3
          ]
      ]
    expected =
      [ Inc 1 1
      , Move 2
      , Inc 1 0
      , In 3 0
      , Out 8 0
      , Out 1 1
      , Out 1 2
      , Loop
          [ Inc 3 0
          , Loop [Move 3, Clear 0, Clear 1, Clear 2]
          , Scan Up 0
          , Scan Down 3
          , Scan Up 0
          , Scan Up 3
          , Scan Down 0
          , Scan Down 3
          ]
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
    fused (Inc _ n) (Inc _ m)
      | n == m = False
    fused (Move _) (Move _) = False
    fused (In _ n) (In _ m)
      | n == m = False
    fused (Out _ n) (Out _ m)
      | n == m = False
    fused (Clear n) (Clear m)
      | n == m = False
    fused (Loop inner) _ = fullyFused $ Program inner
    fused _ (Loop inner) = fullyFused $ Program inner
    fused _ _ = True

unit_fullyFusedTest :: Assertion
unit_fullyFusedTest =
  not (fullyFused $ Program [Inc 1 1, Inc 4 1]) &&
  not (fullyFused $ Program [Inc 1 0, Loop [Inc 1 1, Loop [Move 1, Move 2]]]) &&
  fullyFused (Program [Inc 1 0, Move 2]) @=? True

hprop_fusionDoesntLeaveAnythingToBeFused :: Property
hprop_fusionDoesntLeaveAnythingToBeFused =
  property $ do
    program <- forAll programGen
    let optimized = fusionOpt . toIR $ program
    H.assert $ all noNoOp (flattened optimized) && fullyFused optimized
  where
    noNoOp (Inc n _)    = n /= 0
    noNoOp (Move n)     = n /= 0
    noNoOp (In n _)     = n /= 0
    noNoOp (Out n _)    = n /= 0
    noNoOp (Clear _)    = True
    noNoOp (Mul _ _ to) = to /= 0
    noNoOp (Scan _ _)   = True
    noNoOp (Loop _)     = error "noNoOp: unexpected operation"

hprop_FusedProgramHasValidMonoid :: Property
hprop_FusedProgramHasValidMonoid = property $ HC.monoid programGen

unit_clearOptimization :: Assertion
unit_clearOptimization = clearOpt (Program p) @?= Program expected
  where
    p =
      [ Inc 2 0
      , Inc (-1) 0
      , Loop [Inc (-1) 0]
      , Loop
          [ Inc 1 0
          , Loop [Inc (-1) 0]
          , Loop [Loop [Inc (-1) 0], Move 3, Loop [Inc (-1) 0]]
          ]
      ]
    expected =
      [ Inc 2 0
      , Inc (-1) 0
      , Clear 0
      , Loop [Inc 1 0, Clear 0, Loop [Clear 0, Move 3, Clear 0]]
      ]

unit_mulOptimization :: Assertion
unit_mulOptimization = mulOpt (Program p) @?= Program expected
  where
    p =
      [ Inc 2 0
      , Inc (-1) 0
      , makeMul [(1, 2)]
      , Loop [Inc (-1) 0, makeMul [(2, 4), (4, 5)], Inc 1 0]
      , Loop [Inc (-1) 0, Move 1, Inc 1 0, Move (-1), Inc 1 0] {- this extra part breaks the multiplication loop -}
         -- this tests missing eof
      ]
    expected =
      [ Inc 2 0
      , Inc (-1) 0
      , Mul 1 0 2
      , Clear 0
      , Loop [Inc (-1) 0, Mul 2 0 4, Mul 4 0 9, Clear 0, Inc 1 0]
      , Loop [Inc (-1) 0, Move 1, Inc 1 0, Move (-1), Inc 1 0]
      ]

unit_scanOptimization :: Assertion
unit_scanOptimization = scanOpt (Program p) @?= Program expected
  where
    p =
      [ Inc 2 0
      , Inc (-1) 0
      , Loop [Move (-1)]
      , Loop [Inc (-1) 0, Loop [Move 1], Loop [Move 2], Inc 1 0]
      ]
    expected =
      [ Inc 2 0
      , Inc (-1) 0
      , Scan Down 0
      , Loop [Inc (-1) 0, Scan Up 0, Loop [Move 2], Inc 1 0]
      ]

unit_offsetInstructionsOptimization :: Assertion
unit_offsetInstructionsOptimization =
  offsetInstructionOpt (Program p) @?= Program expected
  where
    p =
      [ Inc 2 0
      , Move 1
      , Inc (-1) 0
      , Move 2
      , Mul 3 0 2
      , Loop [Move (-1)]
      , Loop
          [ Inc (-1) 0
          , Move (-1)
          , In 1 0
          , Loop [Move 2, Inc 3 0, Move (-1)]
          , Inc 1 0
          ]
      ]
    expected =
      [ Inc 2 0
      , Inc (-1) 1
      , Mul 3 3 2
      , Move 3
      , Loop [Move (-1)]
      , Loop [Inc (-1) 0, In 1 (-1), Move (-1), Loop [Inc 3 2, Move 1], Inc 1 0]
      ]
