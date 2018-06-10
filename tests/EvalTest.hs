module EvalTest where

import           Control.Monad.Trans.State (execStateT)
import qualified Data.Text.Lazy.IO         as TIO
import           Test.HUnit

import qualified HBF.Compiler              as C
import qualified HBF.Eval                  as E
import           HBF.Types

unit_canFactorNumbers :: Assertion
unit_canFactorNumbers = do
  code <- TIO.readFile "tests/factor.bf"
  let (Right (program, _)) = C.inMemoryCompile C.defaultCompilerOptions code
  finalState <- exec program
  mockOutputS finalState @?= "25454: 2 11 13 89\n"
  where
    exec program = execStateT (E.eval program) (mkMockIOS "25454\n")
