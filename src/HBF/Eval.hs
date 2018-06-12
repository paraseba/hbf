{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HBF.Eval where

import           Control.Monad               (foldM, replicateM)
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import           Data.Int
import           Data.Maybe                  (fromMaybe)
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Unboxed         as V

import           HBF.Types

eval :: (MachineIO m, PrimMonad m) => OptimizedProgram -> m Tape
eval = evalWithTape emptyTape

evalWithTape :: (MachineIO m, PrimMonad m) => Tape -> OptimizedProgram -> m Tape
evalWithTape Tape {..} program = do
  mv <- V.thaw memory
  finalPointer <- foldM (evalOp mv) pointer program
  finalMemory <- V.unsafeFreeze mv
  return Tape {memory = finalMemory, pointer = finalPointer}

evalOp ::
     forall v m. (MV.MVector v Int8, PrimMonad m, MachineIO m)
  => v (PrimState m) Int8
  -> Int
  -> OptimizedOp
  -> m Int
evalOp v pointer (Inc n) =
  MV.modify v (+ fromIntegral n) pointer >> return pointer
evalOp _ pointer (MRight n) = return $ pointer + n
evalOp v pointer (Out n) =
  MV.read v pointer >>= replicateM n . putByte >> return pointer
evalOp v pointer (In n) =
  input >>= MV.write v pointer . fromMaybe 0 >> return pointer
  where
    input :: m (Maybe Int8)
    input = foldr (flip (>>)) (return Nothing) $ replicate n getByte --fixme return Nothing ugly, we need n > 0
evalOp v pointer (Loop ops) = do
  condition <- MV.read v pointer
  if condition == 0
    then return pointer
    else foldM (evalOp v) pointer ops >>= flip (evalOp v) (Loop ops)

{-# INLINE evalOp #-}
tapeSize :: Int
tapeSize = 30000

emptyTape :: Tape
emptyTape = Tape {memory = V.replicate tapeSize 0, pointer = 0}
