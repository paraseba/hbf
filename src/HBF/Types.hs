{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module HBF.Types where

import Data.Vector.Unboxed (Vector)
import Data.Int
import Data.Binary (Binary)
import GHC.Generics (Generic)
import System.IO (hFlush, stdout)
import Control.Monad.Trans.State.Lazy (StateT, get, put, modify)


data Op =
    Inc
  | Dec
  | MLeft
  | MRight
  | In
  | Out
  | Loop [Op]

  | IncN Int
  | DecN Int
  | MLeftN Int
  | MRightN Int
  | InN Int
  | OutN Int
  deriving (Show, Eq, Generic)

instance Binary Op

type Program = [Op]


data Tape = Tape
  { memory :: Vector Int8
  , pointer :: Int
  }
  deriving (Show)

class MachineIO m where
  putByte :: Int8 -> m ()
  getByte :: m Int8

instance MachineIO IO where
  putByte = putChar . toEnum . fromIntegral
  getByte = fromIntegral . fromEnum <$> (hFlush stdout >> getChar)

data MockIO = MockIO {machineIn :: [Int8], machineOut :: [Int8]}

mkMockIO :: [Int8] -> MockIO
mkMockIO input = MockIO {machineIn = input, machineOut = []}

mockOutput :: MockIO -> [Int8]
mockOutput = reverse . machineOut

instance Monad m => MachineIO (StateT MockIO m) where
  putByte :: Int8 -> StateT MockIO m ()
  putByte b =
    modify update
    where
      update st@MockIO{..} = st {machineOut = b:machineOut}

  getByte :: StateT MockIO m Int8
  getByte = do
    st@MockIO{..} <- get
    let (h:rest) = machineIn
    put st{machineIn = rest}
    return h
