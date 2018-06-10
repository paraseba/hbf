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
unit_fusionOptimization =
  fusionOpt (Program p) @?= Program expected
  where
    p = [IncN 2, IncN (-1), MRightN 3, MRightN (-1),
         IncN 1, InN 1, InN 2, OutN 3, OutN 4,
         IncN 2, IncN (-2), -- should eliminate this
         OLoop [ IncN 1, IncN 2,
           OLoop [MRightN 1, MRightN 2],
           OLoop [MRightN 1, MRightN (-1)] -- should eliminate this whole loop
               ]
        ]
    expected = [IncN 1, MRightN 2, IncN 1, InN 3, OutN 7,
                OLoop [IncN 3, OLoop [MRightN 3]]
               ]


unit_optimizationsDontChangeResults :: Assertion
unit_optimizationsDontChangeResults = do
  code <- TIO.readFile "tests/factor.bf"
  let (Right (optimized, _)) = inMemoryCompile defaultCompilerOptions code
  let (Right (unoptimized, _)) = inMemoryCompile noOptimizationCompilerOptions code
  a <- exec optimized
  b <- exec unoptimized

  a @?= b

  where
    exec program = execStateT (E.eval program) (mkMockIOS "25454\n")

fullyFused :: [OptimizedOp] -> Bool
fullyFused ops =
  all (uncurry fused) (zip (toList ops) (tail (toList ops)))
  where
    fused (IncN _) (IncN _) = False
    fused (MRightN _) (MRightN _) = False
    fused (InN _) (InN _) = False
    fused (OutN _) (OutN _) = False
    fused (OLoop inner) _ = fullyFused inner
    fused _ (OLoop inner) = fullyFused inner
    fused _ _ = True

unit_fullyFusedTest :: Assertion
unit_fullyFusedTest =
  not (fullyFused [IncN 1, IncN 4]) &&
    not (fullyFused [IncN 1, OLoop [IncN 1, OLoop [MRightN 1, MRightN 2]]]) &&
    fullyFused [IncN 1, MRightN 2]  @=? True

hprop_fusionDoesntLeaveAnythingToBeFused :: Property
hprop_fusionDoesntLeaveAnythingToBeFused = property $ do
  program <- forAll programGen
  let optimized = fusionOpt . toIR $ program
  H.assert $
    all noNoOp optimized && fullyFused (instructions optimized)
  where
    noNoOp (IncN n)    = n /= 0
    noNoOp (MRightN n) = n /= 0
    noNoOp (InN n)     = n /= 0
    noNoOp (OutN n)    = n /= 0
    noNoOp (OLoop ops) = all noNoOp ops
