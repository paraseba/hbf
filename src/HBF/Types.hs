{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE RecordWildCards            #-}

{-|
Description : Basic types for the Brainfuck compiler and virtual machine.
Copyright   : (c) Sebastian Galkin, 2018
License     : GPL-3

All the basic types for the Brainfuck compiler and VM are defined in this module.
This includes the different instructions ('Op's), the 'Program' and the 'MachineIO'.
-}
module HBF.Types where

import           Control.DeepSeq                (NFData)
import           Control.Exception              (catch)
import           Control.Monad.Trans.State.Strict (StateT, get, modify, put)
import           Data.Binary                    (Binary)
import           Data.Char                      (chr, ord)
import           Data.Int                       (Int8)
import           Data.List                      (uncons)
import           Data.Semigroup                 (Semigroup (..))
import           GHC.Generics                   (Generic)
import           System.IO                      (hFlush, stdout)

-- * Virtual Machine Instructions
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
  -- | Find the nearest zero in the given direction, starting at the offset position. See 'Direction'.
  | Scan !Direction
         {-# UNPACK #-}!MemOffset
  deriving (Show, Eq, Generic, Binary, NFData)

-- | An offset into the Brainfuck VM memory. Positive numbers are in the direction of higher memory.
newtype MemOffset =
  MemOffset Int
  deriving (Generic)
  deriving newtype (Show, Eq, Num, Ord)
  deriving anyclass (Binary, NFData)

-- | A factor to multiply by in the 'Mul' instruction.
newtype MulFactor =
  MulFactor Int
  deriving (Generic)
  deriving newtype (Show, Eq, Num)
  deriving anyclass (Binary, NFData)

-- | A direction to 'Scan' for a memory position. 'Up' is in the direction of higher memory.
data Direction
  = Up -- ^ Scan in the direction of higher memory.
  | Down -- ^ Scan in the direction of lower memory.
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, NFData)

-- * Programs
-- | Marker type to distinguish optimized and 'Unoptimized' 'Program's.
data Optimized

-- | Marker type to distinguish 'Optimized' and unoptimized 'Program's.
data Unoptimized

-- | A list of 'Op's. 'opt' will be one of 'Optimized' or 'Unoptimized' to
-- distinguish both types of programs at the type level.
newtype Program opt = Program
  { instructions :: [Op] -- ^ The list of instructions in the program.
  } deriving (Generic) deriving newtype (Show, Eq) deriving anyclass ( Binary
                                                                     , NFData
                                                                     )

-- | Return the full list of instructions in a program, by unrolling 'Loop' instructions
-- into the list.
--
-- >>> flattened $ Program [Inc 1 0, Loop [Move 1, Scan Up 0]]
-- [Inc 1 0,Move 1,Scan Up 0]
flattened :: Program o -> [Op]
flattened p = [atom | op <- instructions p, atom <- atoms op]
  where
    atoms (Loop ops) = concatMap atoms ops
    atoms other      = [other]

-- | Apply '<>' to the underlying @List@ of instructions.
instance Semigroup (Program o) where
  Program a <> Program b = Program $ a <> b

-- | The 'Monoid' of the underlying @List@ of instructions.
instance Monoid (Program o) where
  mappend = (<>)
  mempty = Program mempty

-- * Runtime State
-- | The state of a Brainfuck virtual machine.
data Machine v = Machine
  { memory  :: v -- ^ The full memory of the machine. This will be a 'Data.Vector.Unboxed.Vector' or a List.
  , pointer :: MemOffset -- ^ The current execution pointer, information is written and read at this position.
  } deriving (Show, Eq)

-- * VM Input/Output
-- | Provide input and output to a Brainfuck virtual machine.
--
-- This class allows to run the VM in different monads, like 'IO' or 'StateT'.
class MachineIO m where
  putByte :: Int8 -> m () -- ^ Write the byte to the output of the VM.
  getByte :: m (Maybe Int8) -- ^ Read a byte from the input of the VM. If @EOF@ has been reached, return 'Nothing'

-- | 'IO' takes its input and output from stdin/stdout
instance MachineIO IO where
  putByte = putChar . toEnum . fromIntegral
  getByte = fmap (fromIntegral . fromEnum) <$> (hFlush stdout >> safeGetChar)
    where
      safeGetChar = fmap Just getChar `catch` recover
      recover :: IOError -> IO (Maybe Char)
      recover _ = return Nothing

-- * Test Helpers
-- | A data structure for mocking input and output to the VM. This can be used to run the VM
-- in a 'StateT' monad for testing purposes.
data MockIO = MockIO
  { machineIn  :: [Int8]
    -- ^ Every time the machine executes an 'In' instruction, input will be taken from this list.
  , machineOut :: [Int8]
    -- ^ Every time the machine executes an 'Out' instruction, output will be put into this list, in LIFO order.
  } deriving (Show, Eq, Generic, NFData)

-- | Create a 'MockIO' that will have the given input available.
mkMockIO :: [Int8] -> MockIO
mkMockIO input = MockIO {machineIn = input, machineOut = []}

-- | Create a 'MockIO' that will have the given input available. ASCII encoding.
mkMockIOS :: String -> MockIO
mkMockIOS = mkMockIO . map (fromIntegral . ord)

-- | Get the output after a VM has ran using this 'MockIO'.
mockOutput :: MockIO -> [Int8]
mockOutput = reverse . machineOut

-- | Get the output after a VM has ran using this 'MockIO'. ASCII encoding.
mockOutputS :: MockIO -> String
mockOutputS = map (chr . fromIntegral) . mockOutput

-- | 'StateT' takes its input and output from the lists inside the 'MockIO'.
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

-- * Helper Functions
-- | '<$>' with arguments reversed.
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

-- | Helper function to convert a 'Right' into a 'Just' and a 'Left' into a 'Nothing'.
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe (Left _)  = Nothing
