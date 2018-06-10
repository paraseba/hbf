module CompilerTest where

import           Test.HUnit
import qualified Data.Text.Lazy.IO    as TIO
import           Control.Monad.Trans.State (execStateT)

import HBF.Compiler
import qualified HBF.Eval as E
import           HBF.Types

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
