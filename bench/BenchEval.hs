module Main where

import           Control.Monad.Trans.State (execStateT)
import           Criterion.Main
import qualified Data.Text.Lazy.IO         as TIO

import           HBF.Compiler              as C
import qualified HBF.Eval                  as E
import           HBF.Types

setupEnv :: IO (OptimizedProgram, OptimizedProgram)
setupEnv = do
  code <- TIO.readFile "tests/factor.bf"
  let (Right (optimized, _)) = C.inMemoryCompile C.defaultCompilerOptions code
  let (Right (unoptimized, _)) =
        inMemoryCompile noOptimizationCompilerOptions code
  return (optimized, unoptimized)

factorInput :: MockIO
factorInput = mkMockIOS "25454\n"

main :: IO ()
main =
  defaultMain
    [ env setupEnv $ \ ~(optimized, unoptimized) ->
        bgroup
          "factor"
          [ bench "fully optimized" $
            whnfIO $ execStateT (E.eval optimized) factorInput
          , bench "not optimized" $
            whnfIO $ execStateT (E.eval unoptimized) factorInput
          ]
    ]
