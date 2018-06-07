{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module HBF.Eval
where

import Data.Int
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Unboxed as V
import Control.Monad (foldM)
import Control.Monad.Primitive (PrimMonad, PrimState)
import HBF.Types

eval :: (MachineIO m, PrimMonad m) => Program -> m Tape
eval = evalWithTape emptyTape

evalWithTape :: (MachineIO m, PrimMonad m) => Tape -> Program -> m Tape
evalWithTape Tape{..} program = do
  mv <- V.unsafeThaw memory
  finalPointer <- foldM (evalOp mv) pointer program
  finalMemory <- V.freeze mv
  return Tape {memory = finalMemory, pointer = finalPointer}

evalOp :: (MV.MVector v Int8, PrimMonad m, MachineIO m) => v (PrimState m) Int8 -> Int -> Op -> m Int

evalOp v pointer Inc =
  MV.modify v (+1) pointer >> return pointer

evalOp v pointer Dec =
  MV.modify v (subtract 1) pointer >> return pointer

evalOp _ pointer MLeft =
  return $ pointer - 1

evalOp _ pointer MRight =
  return $ pointer + 1

evalOp v pointer Out =
  MV.read v pointer >>= putByte >> return pointer

evalOp v pointer In =
  getByte >>= MV.write v pointer >> return pointer

evalOp v pointer (Loop ops) = do
  condition <- MV.read v pointer
  if condition == 0
    then return pointer
    else foldM (evalOp v) pointer ops >>= flip (evalOp v) (Loop ops)

{-# INLINE evalOp #-}

tapeSize :: Int
tapeSize = 30000

emptyTape :: Tape
emptyTape = Tape
  { memory = V.replicate tapeSize 0
  , pointer = 0
  }
