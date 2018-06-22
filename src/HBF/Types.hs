{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE RecordWildCards            #-}

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

-- | Operations or instructions in the Brainfuck virtual machine.
--
-- Some of these operations are \"native\" to Brainfuck and others are the result of optimization during compilation.
-- The compiler generates these types of instructions and the virtual machine can execute them.
--
-- In all these instructions the 'MemOffset' represents a shift relative to the current position of the pointer.
-- The operation will refer and apply its action to this shifted position.
data Op
  -- | Increment by the amount specified by the @Int@
  = Inc {-# UNPACK #-}!Int
        {-# UNPACK #-}!MemOffset
  -- | Move the current pointer by the specified amount
  | Move {-# UNPACK #-}!MemOffset
  -- | Repeatedly read a byte into the machine and write the last one read to the shifted position.
  -- @n@ is usually 1 in real programs, but not always. Where the byte is read from will depend on the 'MachineIO' impleentation.
  | In {-# UNPACK #-}!Int
       {-# UNPACK #-}!MemOffset
  -- | Repeatedly write the byte in the shifted position. Where the byte is written will depend on the 'MachineIO' impleentation.
  | Out {-# UNPACK #-}!Int
        {-# UNPACK #-}!MemOffset
  -- | Native Brainfuck looping instruction.
  | Loop ![Op]
  -- | Optimized instruction. Set the shifted position to zero. In Brainfuck this is usually written as @[-]@
  | Clear {-# UNPACK #-}!MemOffset
  -- | Optimized instruction. Multiply by the factor the byte in the first @MemOffset@, writting to the second one.
  -- Second @MemOffset@ is relative to the first one. In brainfuck this is usually written as [->+<] and similar
  -- expressions.
  | Mul {-# UNPACK #-}!MulFactor
        {-# UNPACK #-}!MemOffset
        {-# UNPACK #-}!MemOffset
  -- | Find the nearest zero in the given direction, starting at the offset position.
  | Scan !Direction
         {-# UNPACK #-}!MemOffset
  deriving (Show, Eq, Generic, Binary, NFData)

newtype MemOffset =
  MemOffset Int
  deriving (Generic)
  deriving newtype (Show, Eq, Num, Ord)
  deriving anyclass (Binary, NFData)

newtype MulFactor =
  MulFactor Int
  deriving (Generic)
  deriving newtype (Show, Eq, Num)
  deriving anyclass (Binary, NFData)

data Direction
  = Up
  | Down
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, NFData)

data Optimized

data Unoptimized

newtype Program optimized = Program
  { instructions :: [Op]
  } deriving (Generic) deriving newtype (Show, Eq) deriving anyclass ( Binary
                                                                     , NFData
                                                                     )

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
  , pointer :: MemOffset
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
  } deriving (Show, Eq, Generic, NFData)

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
