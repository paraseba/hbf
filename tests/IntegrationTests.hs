{-# LANGUAGE OverloadedStrings #-}

module IntegrationTests where

import           Control.Monad.Trans.State (execStateT)
import           System.IO                 (hClose)
import           System.IO.Temp            (withSystemTempFile)
import           Test.HUnit

import qualified HBF.Compiler              as C
import qualified HBF.Eval                  as E
import           HBF.Types


squaresPath :: FilePath
squaresPath = "tests/squares.bf"

squaresResult :: String
squaresResult =
  concatMap ((++"\n"). show) sq
  where
    sq :: [Int]
    sq = takeWhile (<=10000) $ map (\n -> n * n) [0..]

compile :: FilePath -> IO OptimizedProgram
compile inpath =
  withSystemTempFile "hbftests.bfc" $ \outpath handle -> do
    hClose handle
    cOpts <- C.unsafeParse ["--output", outpath, inpath]
    Right _ <- C.compile cOpts
    C.loadFile outpath

unit_compileAndEvalSquares :: Assertion
unit_compileAndEvalSquares = do
  program <- compile squaresPath
  mock <- execStateT (E.eval program) (mkMockIO [])
  mockOutputS mock @?= squaresResult
