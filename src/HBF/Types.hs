{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
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
import           Data.Vector.Unboxed            (Vector)
import           GHC.Generics                   (Generic)
import           System.IO                      (hFlush, stdout)

data BasicOp
  = BInc
  | BDec
  | BLeft
  | BRight
  | BIn
  | BOut
  | BLoop [BasicOp]
  deriving (Show, Eq)

data OptimizedOp
  = Inc Int
  | MRight Int
  | In Int
  | Out Int
  | Loop [OptimizedOp]
  deriving (Show, Eq, Generic, Binary, NFData)

newtype Program op = Program
  { instructions :: [op]
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic, Binary, NFData)

instance Semigroup (Program op) where
  Program a <> Program b = Program $ a <> b

instance Monoid (Program op) where
  mappend = (<>)
  mempty = Program mempty

type UnoptimizedProgram = Program BasicOp

type OptimizedProgram = Program OptimizedOp

data Tape = Tape
  { memory  :: Vector Int8
  , pointer :: Int
  } deriving (Show)

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
