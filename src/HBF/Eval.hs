module HBF.Eval
where

import Data.Int
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as MV
import Data.Vector (Vector)
import Control.Monad (foldM)
import System.IO (hFlush, stdout)
import HBF.Types

tapeRead :: Tape -> Int8
tapeRead t =
  memory t V.! pointer t

tapeModify :: (Int8 -> Int8) -> Tape -> Tape
tapeModify f t =
  t {
    memory = V.modify ( \mv ->
      MV.modify mv f (pointer t))
      (memory t)
    }

moveTapeRight :: Int -> Tape -> Tape
moveTapeRight n t =
  t {pointer = pointer t + n}

eval :: Program -> IO Tape
eval = evalWith emptyTape

evalWith :: Tape -> Program -> IO Tape
evalWith tape p =
  foldM evalOp tape p

evalOp :: Tape -> Op -> IO Tape
evalOp t Inc = return $ tapeModify (+1) t
evalOp t Dec = return $ tapeModify (+(-1)) t
evalOp t MLeft = return $ moveTapeRight (-1) t
evalOp t MRight = return $ moveTapeRight 1 t
evalOp t In = (\x -> tapeModify (\_ -> x) t) . fromIntegral . fromEnum <$> (hFlush stdout >> getChar)
evalOp t Out = (putChar . toEnum . fromIntegral) (tapeRead t)  >> return t
evalOp t (Loop ops) =
  if (tapeRead t /= 0)
    then  evalWith t ops >>= \tape -> evalOp tape (Loop ops)
    else (return t)

tapeSize :: Int
tapeSize = 100 -- fixme

emptyTape :: Tape
emptyTape = Tape
  { memory = V.replicate tapeSize 0
  , pointer = 0
  }
