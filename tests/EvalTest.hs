module EvalTest where

import           Control.Monad.Trans.State (runStateT)
import qualified Data.Text.Lazy.IO         as TIO
import           Test.HUnit

import qualified HBF.Compiler              as C
import qualified HBF.Eval                  as E
import           HBF.Types
import Helper

exec :: Program Optimized -> MockIO -> IO (E.TapeType, MockIO)
exec program = runStateT (E.eval program)

unit_canFactorNumbers :: Assertion
unit_canFactorNumbers = do
  code <- TIO.readFile "tests/factor.bf"
  let (Right (program, _)) = C.inMemoryCompile C.defaultCompilerOptions code
  (_,finalState) <- exec program (mkMockIOS "25454\n")
  mockOutputS finalState @?= "25454: 2 11 13 89\n"

unit_evalScanR :: Assertion
unit_evalScanR = do
  (tape, _) <- exec program (mkMockIO [])
  let (Tape mem index) = listTape tape
  (index, take 6 mem) @?= (4, [42,42,42,42,17,0])
  where
    program = -- evaluating should give [42,42,42,42,17,0,...]
      Program [Inc 42, Mul 1 1, Mul 2 1, Mul 3 1, ScanR, Inc 17]

unit_evalScanL :: Assertion
unit_evalScanL = do
  (tape, _) <- exec program (mkMockIO [])
  let (Tape mem index) = listTape tape
  (index, take 6 mem) @?= (1, [0,17,42,42,42,0])
  where
    program = -- evaluating should give [0,17,42,42,42,0,...]
      Program [MRight 2, Inc 42, Mul 1 1, Mul 2 1, ScanL, Inc 17]

unit_evalScanROnZero :: Assertion
unit_evalScanROnZero = do
  (tape, _) <- exec program (mkMockIO [])
  let (Tape mem index) = listTape tape
  (index, take 4 mem) @?= (1, [42,0,42,0])
  where
    program = -- evaluating should give [42,0,42,0,...]
      Program [Inc 42, MRight 2, Inc 42, MRight (-1), ScanR]

unit_evalScanLOnZero :: Assertion
unit_evalScanLOnZero = do
  (tape, _) <- exec program (mkMockIO [])
  let (Tape mem index) = listTape tape
  (index, take 4 mem) @?= (1, [42,0,42,0])
  where
    program = -- evaluating should give [42,0,42,0,...]
      Program [Inc 42, MRight 2, Inc 42, MRight (-1), ScanL]

-- todo: implement a property test starting with arbitrary memory
-- and evaluating ScanR and ScanL
