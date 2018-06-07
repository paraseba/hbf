{-# LANGUAGE DeriveGeneric #-}

module HBF.Types where

import Data.Vector.Unboxed (Vector)
import Data.Int
import Data.Binary
import GHC.Generics (Generic)
import System.IO (hFlush, stdout)

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
