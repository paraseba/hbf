{-# LANGUAGE DeriveGeneric #-}

module HBF.Types where

import Data.Vector (Vector)
import Data.Int
import Data.Binary
import GHC.Generics (Generic)

data Op =
    Inc
  | Dec
  | MLeft
  | MRight
  | In
  | Out
  | Loop [Op]
  deriving (Show, Generic)

instance Binary Op

type Program = [Op]


data Tape = Tape
  { memory :: Vector Int8
  , pointer :: Int
  }
  deriving (Show)
