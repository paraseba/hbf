module HBF.Types where

import Data.Vector (Vector)
import Data.Int

data Op =
    Inc
  | Dec
  | MLeft
  | MRight
  | In
  | Out
  | Loop [Op]
  deriving Show

type Program = [Op]

data Tape = Tape
  { memory :: Vector Int8
  , pointer :: Int
  }
  deriving (Show)
