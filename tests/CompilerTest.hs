module CompilerTest where

import           Test.HUnit
import qualified Data.Text.Lazy.IO    as TIO
import           Control.Monad.Trans.State (execStateT)

import HBF.Compiler
import qualified HBF.Eval as E
import           HBF.Types

unit_optimizationsDontChangeResults :: Assertion
unit_optimizationsDontChangeResults = do
  code <- TIO.readFile "tests/factor.bf"
  let (Right optimized) = inMemoryCompile defaultCompilerOptions code
  let (Right unoptimized) = inMemoryCompile noOptimizationCompilerOptions code
  a <- exec optimized
  b <- exec unoptimized

  a @?= b

  where
    exec program = execStateT (E.eval program) (mkMockIOS "25454\n")
