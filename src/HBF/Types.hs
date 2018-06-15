{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE RecordWildCards   #-}

module HBF.Types where

import           Control.DeepSeq                (NFData)
import           Control.Exception              (catch)
import           Control.Monad.Trans.State.Lazy (StateT, get, modify, put)
import           Data.Binary                    (Binary)
import           Data.Char                      (chr, ord)
import           Data.Int
import           Data.List                      (uncons)
import           Data.Semigroup                 (Semigroup (..))
import           GHC.Generics                   (Generic)
import           System.IO                      (hFlush, stdout)

data Op
  = Inc Int
  | MRight Int
  | In Int
  | Out Int
  | Loop [Op]
  | Clear
  | Mul MulOffset
        MulFactor
  | ScanR
  | ScanL
  deriving (Show, Eq, Generic, Binary, NFData)

newtype MulOffset = MulOffset Int
  deriving Generic
  deriving newtype (Show, Eq, Num)
  deriving anyclass (Binary, NFData)

newtype MulFactor = MulFactor Int
  deriving Generic
  deriving newtype (Show, Eq, Num)
  deriving anyclass (Binary, NFData)

data Optimized

data Unoptimized

newtype Program optimized = Program
  { instructions :: [Op]
  }
  deriving Generic
  deriving newtype (Show, Eq)
  deriving anyclass (Binary, NFData)

flattened :: Program o -> [Op]
flattened p = [atom | op <- instructions p, atom <- atoms op]
  where
    atoms (Loop ops) = concatMap atoms ops
    atoms other      = [other]

instance Semigroup (Program o) where
  Program a <> Program b = Program $ a <> b

instance Monoid (Program o) where
  mappend = (<>)
  mempty = Program mempty

data Tape v = Tape
  { memory  :: v
  , pointer :: Int
  } deriving (Show, Eq)

class MachineIO m where
  putByte :: Int8 -> m ()
  getByte :: m (Maybe Int8)

instance MachineIO IO where
  putByte = putChar . toEnum . fromIntegral
  getByte = fmap (fromIntegral . fromEnum) <$> (hFlush stdout >> safeGetChar)
    where
      safeGetChar = fmap Just getChar `catch` recover
      recover :: IOError -> IO (Maybe Char)
      recover _ = return Nothing

-- fixme move to test helper

data MockIO = MockIO
  { machineIn  :: [Int8]
  , machineOut :: [Int8]
  } deriving (Show, Eq)

mkMockIO :: [Int8] -> MockIO
mkMockIO input = MockIO {machineIn = input, machineOut = []}

mkMockIOS :: String -> MockIO
mkMockIOS = mkMockIO . map (fromIntegral . ord)

mockOutput :: MockIO -> [Int8]
mockOutput = reverse . machineOut

mockOutputS :: MockIO -> String
mockOutputS = map (chr . fromIntegral) . mockOutput

instance Monad m => MachineIO (StateT MockIO m) where
  putByte :: Int8 -> StateT MockIO m ()
  putByte b = modify update
    where
      update st@MockIO {..} = st {machineOut = b : machineOut}
  getByte :: StateT MockIO m (Maybe Int8)
  getByte = do
    st@MockIO {..} <- get
    maybe (pure Nothing) (update st) $ uncons machineIn
    where
      update st (b, bs) = put st {machineIn = bs} >> return (Just b)

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe (Left _)  = Nothing
