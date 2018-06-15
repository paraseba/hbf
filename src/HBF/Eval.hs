{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HBF.Eval where

import           Control.Monad               (foldM, replicateM)
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import           Data.Int
import           Data.Maybe                  (fromMaybe, fromJust)
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Generic         as GV
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Fusion.Stream.Monadic as VStream

import           HBF.Types

type TapeType = Tape (V.Vector Int8)

eval :: (MachineIO m, PrimMonad m) => Program Optimized -> m TapeType
eval = evalWithTape emptyTape

evalWithTape ::
  (MachineIO m, PrimMonad m, GV.Vector v Int8) => Tape (v Int8) -> Program Optimized -> m (Tape (v Int8))
evalWithTape Tape {..} program = do
  mv <- GV.thaw memory
  finalPointer <- foldM (evalOp mv) pointer (instructions program)
  finalMemory <- GV.unsafeFreeze mv
  return Tape {memory = finalMemory, pointer = finalPointer}

evalOp ::
     forall v m. (MV.MVector v Int8, PrimMonad m, MachineIO m)
  => v (PrimState m) Int8
  -> Int
  -> Op
  -> m Int
evalOp v pointer (Inc n) =
  MV.modify v (+ fromIntegral n) pointer >> return pointer
evalOp _ pointer (MRight n) = return $ pointer + n
evalOp v pointer (Out n) =
  MV.read v pointer >>= replicateM n . putByte >> return pointer
evalOp v pointer (In n) =
  if n == 0
    then return pointer
    else input >>= MV.write v pointer . fromMaybe 0 >> return pointer
  where
    input :: m (Maybe Int8)
    input = foldr (flip (>>)) (return Nothing) $ replicate n getByte
evalOp v pointer (Loop ops) = do
  condition <- MV.read v pointer
  if condition == 0
    then return pointer
    else foldM (evalOp v) pointer ops >>= flip (evalOp v) (Loop ops)
evalOp v pointer Clear = MV.write v pointer 0 >> return pointer
evalOp v pointer (Mul (MulOffset offset) (MulFactor factor)) = do
  value <- MV.read v pointer
  MV.modify v (\old -> old + value * fromIntegral factor) (pointer + offset)
  return pointer
evalOp v pointer ScanR = do
   (pointer+) . fromJust <$> VStream.findIndex (==0)  (MV.mstream slice)   -- todo error handling
  where
    slice :: v (PrimState m) Int8
    slice = MV.slice pointer (MV.length v - pointer) v
evalOp v pointer ScanL = do
   (pointer -) . fromJust <$> VStream.findIndex (==0)  (MV.mstreamR slice)   -- todo error handling
  where
    slice :: v (PrimState m) Int8
    slice = MV.slice 0 (pointer + 1) v


tapeSize :: Int
tapeSize = 30000

emptyTape :: TapeType
emptyTape = Tape {memory = GV.replicate tapeSize 0, pointer = 0}
