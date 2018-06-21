{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

module HBF.Eval
  ( eval
  , evalWithTape
  , TapeType
  ) where

import           Control.Monad                     (replicateM_)
import           Control.Monad.Primitive           (PrimMonad, PrimState)
import           Data.Coerce                       (coerce)
import           Data.Int                          (Int8)
import           Data.Maybe                        (fromMaybe)
import qualified Data.Vector.Fusion.Stream.Monadic as VStream
import qualified Data.Vector.Generic               as GV
import qualified Data.Vector.Generic.Mutable       as MV
import qualified Data.Vector.Unboxed

import           HBF.Types

type TapeType = Tape (Data.Vector.Unboxed.Vector Int8)

{-# SPECIALISE evalWithTape ::
                 TapeType -> Program Optimized -> IO TapeType #-}

{-# INLINABLE evalWithTape #-}
{-# INLINABLE eval #-}
eval :: (PrimMonad m, MachineIO m) => Program Optimized -> m TapeType
eval = evalWithTape emptyTape

evalWithTape ::
     forall m. (PrimMonad m, MachineIO m)
  => TapeType
  -> Program Optimized
  -> m TapeType
evalWithTape Tape {..} program = do
  mem <- GV.thaw memory
  finalPointer <- mutableEval (instructions program) mem 0
  finalMemory <- GV.unsafeFreeze mem
  return Tape {memory = finalMemory, pointer = finalPointer}
  -- For some reason making this function a top level binding brings down performance by compiling
  -- without native arithmetic. Even if we add SPECIALIZE pragma
  -- Maybe this is the reason why we also need -fno-full-laziness
  where
    mutableEval ::
         forall v. (MV.MVector v Int8)
      => [Op]
      -> v (PrimState m) Int8
      -> MemOffset
      -> m MemOffset
    mutableEval [] _ pos = return pos
    mutableEval (op:ops) mem pos =
      case op of
        Inc n memOffset ->
          MV.unsafeModify mem (+ fromIntegral n) (o2i $ pos + memOffset) *>
          mutableEval ops mem pos
        MRight n -> mutableEval ops mem (pos + coerce n)
        Out times memOffset -> do
          val <- MV.unsafeRead mem (o2i $ pos + memOffset)
          replicateM_ times (putByte val)
          mutableEval ops mem pos
        In times memOffset ->
          if times == 0
            then mutableEval ops mem pos
            else let input :: m (Maybe Int8)
                     input =
                       foldr (flip (*>)) (return Nothing) $
                       replicate times getByte
                  in do input >>=
                          MV.write mem (o2i $ pos + memOffset) . fromMaybe 0
                        mutableEval ops mem pos
        Loop l -> do
          let go pos' = do
                condition <- MV.unsafeRead mem (o2i pos')
                if condition == 0
                  then mutableEval ops mem pos'
                  else (do pos'' <- mutableEval l mem pos'
                           go pos'')
          go pos
        Clear offset ->
          MV.unsafeWrite mem (o2i $ pos + offset) 0 *> mutableEval ops mem pos
        Mul factor from to -> do
          value <- MV.unsafeRead mem (o2i $ pos + from)
          MV.unsafeModify
            mem
            (\old -> old + value * factor2i factor)
            (o2i $ pos + from + to)
          mutableEval ops mem pos
        Scan Up offset ->
          let start = o2i $ pos + offset
              slice :: v (PrimState m) Int8
              slice = MV.slice start (MV.length mem - start) mem
           in do Just idx <- VStream.findIndex (== 0) (MV.mstream slice) -- todo error handling
                 mutableEval ops mem (MemOffset $ start + idx)
        Scan Down offset ->
          let end = o2i $ pos + offset
              slice :: v (PrimState m) Int8
              slice = MV.slice 0 (end + 1) mem
           in do Just idx <- VStream.findIndex (== 0) (MV.mstreamR slice) -- todo error handling
                 mutableEval ops mem (MemOffset $ end - idx)

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
