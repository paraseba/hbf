{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HBF.Eval where

import           Control.Monad                     (foldM, replicateM)
import           Control.Monad.Primitive           (PrimMonad, PrimState)
import           Data.Coerce                       (coerce)
import           Data.Int
import           Data.Maybe                        (fromJust, fromMaybe)
import qualified Data.Vector.Fusion.Stream.Monadic as VStream
import qualified Data.Vector.Generic               as GV
import qualified Data.Vector.Generic.Mutable       as MV
import qualified Data.Vector.Unboxed               as V

import           HBF.Types

type TapeType = Tape (V.Vector Int8)

eval :: (MachineIO m, PrimMonad m) => Program Optimized -> m TapeType
eval = evalWithTape emptyTape

evalWithTape ::
     (MachineIO m, PrimMonad m, GV.Vector v Int8)
  => Tape (v Int8)
  -> Program Optimized
  -> m (Tape (v Int8))
evalWithTape Tape {..} program = do
  mv <- GV.thaw memory
  finalPointer <- foldM (evalOp mv) pointer (instructions program)
  finalMemory <- GV.unsafeFreeze mv
  return Tape {memory = finalMemory, pointer = finalPointer}

evalOp ::
     forall v m. (MV.MVector v Int8, PrimMonad m, MachineIO m)
  => v (PrimState m) Int8
  -> MemOffset
  -> Op
  -> m MemOffset
evalOp v pointer (Inc n memOffset) =
  MV.modify v (+ fromIntegral n) (o2i $ pointer + memOffset) >> return pointer
evalOp _ pointer (MRight n) = return $ pointer + n
evalOp v pointer (Out n memOffset) =
  MV.read v (o2i $ pointer + memOffset) >>= replicateM n . putByte >>
  return pointer
evalOp v pointer (In n memOffset) =
  if n == 0
    then return pointer
    else input >>= MV.write v (o2i $ pointer + memOffset) . fromMaybe 0 >>
         return pointer
  where
    input :: m (Maybe Int8)
    input = foldr (flip (>>)) (return Nothing) $ replicate n getByte
evalOp v pointer (Loop ops) = do
  condition <- MV.read v (o2i pointer)
  if condition == 0
    then return pointer
    else foldM (evalOp v) pointer ops >>= flip (evalOp v) (Loop ops)
evalOp v pointer (Clear offset) =
  MV.write v (o2i $ pointer + offset) 0 >> return pointer
evalOp v pointer (Mul factor offset) = do
  value <- MV.read v (o2i pointer)
  MV.modify v (\old -> old + value * factor2i factor) (o2i $ pointer + offset)
  return pointer
evalOp v pointer (Scan Up offset) =
  (start +) . coerce . fromJust <$> VStream.findIndex (== 0) (MV.mstream slice) -- todo error handling
  where
    start = pointer + offset
    slice :: v (PrimState m) Int8
    slice = MV.slice (o2i start) (MV.length v - o2i pointer) v
evalOp v pointer (Scan Down offset) =
  (end -) . coerce . fromJust <$> VStream.findIndex (== 0) (MV.mstreamR slice) -- todo error handling
  where
    end = pointer + offset
    slice :: v (PrimState m) Int8
    slice = MV.slice 0 (o2i $ end + 1) v

o2i :: MemOffset -> Int
o2i = coerce

{-# INLINE o2i #-}
factor2i :: MulFactor -> Int8
factor2i = fromIntegral . (coerce :: MulFactor -> Int)

{-# INLINE factor2i #-}
tapeSize :: Int
tapeSize = 30000

emptyTape :: TapeType
emptyTape = Tape {memory = GV.replicate tapeSize 0, pointer = 0}
