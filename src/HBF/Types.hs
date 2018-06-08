{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE RecordWildCards   #-}

module HBF.Types where

import           Control.Exception              (catch)
import           Control.Monad.Trans.State.Lazy (StateT, get, modify, put)
import           Data.Binary                    (Binary)
import           Data.Char                      (chr, ord)
import           Data.Int
import           Data.List                      (uncons)
import           Data.Vector.Unboxed            (Vector)
import           GHC.Generics                   (Generic)
import           System.IO                      (hFlush, stdout)

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
  { memory  :: Vector Int8
  , pointer :: Int
  }
  deriving (Show)

class MachineIO m where
  putByte :: Int8 -> m ()
  getByte :: m (Maybe Int8)

instance MachineIO IO where
  putByte = putChar . toEnum . fromIntegral
  getByte = fmap (fromIntegral . fromEnum) <$> (hFlush stdout >> safeGetChar)
    where
      safeGetChar =
        fmap Just getChar `catch` recover

      recover :: IOError -> IO (Maybe Char)
      recover _ = return Nothing

data MockIO = MockIO {machineIn :: [Int8], machineOut :: [Int8]} deriving (Show, Eq)

mkMockIO :: [Int8] -> MockIO
mkMockIO input = MockIO {machineIn = input, machineOut = []}

mkMockIOS :: String -> MockIO
mkMockIOS =
  mkMockIO . map (fromIntegral . ord)


mockOutput :: MockIO -> [Int8]
mockOutput = reverse . machineOut

mockOutputS :: MockIO -> String
mockOutputS = map (chr . fromIntegral) . mockOutput

instance Monad m => MachineIO (StateT MockIO m) where
  putByte :: Int8 -> StateT MockIO m ()
  putByte b =
    modify update
    where
      update st@MockIO{..} = st {machineOut = b:machineOut}

  getByte :: StateT MockIO m (Maybe Int8)
  getByte = do
    st@MockIO{..} <- get
    maybe (pure Nothing) (update st) $ uncons machineIn
    where
      update st (b, bs) = put st{machineIn = bs} >> return (Just b)
