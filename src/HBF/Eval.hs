{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HBF.Eval
where

import           Control.Monad               (foldM, replicateM)
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import           Data.Int
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Unboxed         as V

import           HBF.Types

eval :: (MachineIO m, PrimMonad m) => Program -> m Tape
eval = evalWithTape emptyTape

evalWithTape :: (MachineIO m, PrimMonad m) => Tape -> Program -> m Tape
evalWithTape Tape{..} program = do
  mv <- V.thaw memory
  finalPointer <- foldM (evalOp mv) pointer program
  finalMemory <- V.unsafeFreeze mv
  return Tape {memory = finalMemory, pointer = finalPointer}

evalOp
  :: forall v m. (MV.MVector v Int8, PrimMonad m, MachineIO m)
  => v (PrimState m) Int8 -> Int -> Op -> m Int

evalOp v pointer Inc =
  MV.modify v (+1) pointer >> return pointer

evalOp v pointer (IncN n) =
  MV.modify v (+ fromIntegral n) pointer >> return pointer

evalOp v pointer Dec =
  MV.modify v (subtract 1) pointer >> return pointer

evalOp v pointer (DecN n) =
  MV.modify v (subtract $ fromIntegral n) pointer >> return pointer

evalOp _ pointer MLeft =
  return $ pointer - 1

evalOp _ pointer (MLeftN n) =
  return $ pointer - n

evalOp _ pointer MRight =
  return $ pointer + 1

evalOp _ pointer (MRightN n) =
  return $ pointer + n

evalOp v pointer Out =
  MV.read v pointer >>= putByte >> return pointer

evalOp v pointer (OutN n) =
  MV.read v pointer >>= replicateM n . putByte >> return pointer

evalOp v pointer In =
  getByte >>= MV.write v pointer >> return pointer

evalOp v pointer (InN n) =
  input >>= MV.write v pointer >> return pointer
  where
    input :: m Int8
    input = foldr (>>) (return 0) $ replicate n getByte --fixme return 0 ugly, we need n > 0

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
