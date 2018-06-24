module Main where

import           Control.Monad.Trans.State (execStateT)
import           Criterion.Main
import qualified Data.Text.Lazy.IO         as TIO
import           System.FilePath           (takeBaseName)

import           HBF.Compiler              as C
import qualified HBF.Eval                  as E
import           HBF.Types

type BenchmarkName = String

type ProgramInput = MockIO

type UnoptimizedP = Program Optimized

type OptimizedP = Program Optimized

programEnv ::
     FilePath
  -> String
  -> IO (BenchmarkName, ProgramInput, OptimizedP, UnoptimizedP)
programEnv file input = do
  code <- TIO.readFile file
  let (Right (optimized, _)) = C.inMemoryCompile C.defaultCompilerOptions code
  let (Right (unoptimized, _)) =
        inMemoryCompile noOptimizationCompilerOptions code
  return (takeBaseName file, mkMockIOS input, optimized, unoptimized)

mkProgramGroup ::
     (BenchmarkName, ProgramInput, OptimizedP, UnoptimizedP) -> Benchmark
mkProgramGroup ~(name, input, optimized, unoptimized) =
  bgroup
    name
    [ bench "fully optimized" $ whnfIO $ execStateT (E.eval optimized) input
    , bench "not optimized" $ whnfIO $ execStateT (E.eval unoptimized) input
    ]

main :: IO ()
main =
  defaultMain
    [ bgroup
        "programs"
        [ env (programEnv "tests/factor.bf" "254504\n") mkProgramGroup
        , env (programEnv "tests/allfeatures.bf" "0") mkProgramGroup
        ]
    ]
