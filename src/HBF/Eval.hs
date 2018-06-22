{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

{-|
Description : Brainfuck Virtual Machine
Copyright   : (c) Sebastian Galkin, 2018
License     : GPL-3

Functions to evaluate a compiled Brainfuck program.
-}
module HBF.Eval
  ( MachineType
  , eval
  , evalWith
  , evalWithIO
  , evalWithMachine
  , emptyMachine
  , mkMachine
  , VMOptions(..)
  , defaultVMOptions
  , unsafeParse
  , parse
  , parsePure
  ) where

import           Control.Monad                     (replicateM_, when)
import           Control.Monad.Primitive           (PrimMonad, PrimState)
import           Data.Coerce                       (coerce)
import           Data.Int                          (Int8)
import           Data.Maybe                        (fromMaybe)
import           Data.Monoid                       ((<>))
import qualified Data.Vector.Fusion.Stream.Monadic as VStream
import qualified Data.Vector.Generic               as GV
import qualified Data.Vector.Generic.Mutable       as MV
import qualified Data.Vector.Unboxed
import           Options.Applicative
import           System.Environment                (getArgs)

import           HBF.Types

-- | An alias for a 'Machine' in which memory is an unboxed vector of bytes.
type MachineType = Machine (Data.Vector.Unboxed.Vector Int8)

{-# INLINABLE eval #-}
-- | Evaluate the given program returning the end state of the 'Machine'. The evaluation can
-- happen in any 'PrimMonad' for which we can do I/O. The reason to use 'PrimState' is that
-- we will use mutable vectors for the evaluation.
eval :: (PrimMonad m, MachineIO m) => Program Optimized -> m MachineType
eval = evalWithMachine defaultVMOptions emptyMachine

{-# INLINABLE evalWith #-}
-- | Evaluate the given program returning the end state of the 'Machine'. The evaluation can
-- happen in any 'PrimMonad' for which we can do I/O. The reason to use 'PrimState' is that
-- we will use mutable vectors for the evaluation. 'VMOptions' are used to tune the details
-- of the VM, like available memory, verbosity, etc.
evalWith ::
     (PrimMonad m, MachineIO m)
  => VMOptions
  -> Program Optimized
  -> m MachineType
evalWith opts program =
  evalWithMachine opts (mkMachine (vmOptsMemoryBytes opts)) program

{-# INLINABLE evalWithIO #-}
-- | Evaluate the given program returning the end state of the 'Machine'. The evaluation
-- happens in IO, so Input/Output is done to the console.
evalWithIO :: VMOptions -> Program Optimized -> IO MachineType
evalWithIO opts program = do
  machine <- evalWith opts program
  when (vmOptsDumpMemory opts) $ print machine
  return machine

{-# SPECIALISE evalWithMachine ::
                 VMOptions -> MachineType -> Program Optimized -> IO MachineType #-}

{-# INLINABLE evalWithMachine #-}
-- | Evaluate the given program returning the end state of the 'Machine'. The evaluation can
-- happen in any 'PrimMonad' for which we can do I/O. The reason to use 'PrimState' is that
-- we will use mutable vectors for the evaluation. 'VMOptions' are used to tune the details
-- of the VM, like memory available, verbosity, etc. The evaluation starts with the specified
-- 'MachineType', so the memory and initial pointer can be configured before running.
evalWithMachine ::
     forall m. (PrimMonad m, MachineIO m)
  => VMOptions
  -> MachineType
  -> Program Optimized
  -> m MachineType
evalWithMachine _ Machine {..} program = do
  mem <- GV.thaw memory
  finalPointer <- mutableEval (instructions program) mem 0
  finalMemory <- GV.unsafeFreeze mem
  return Machine {memory = finalMemory, pointer = finalPointer}
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
        Move n -> mutableEval ops mem (pos + coerce n)
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
          x <- MV.unsafeRead mem (o2i $ pos + from)
          MV.unsafeModify
            mem
            (\old -> old + x * factor2i factor)
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
-- | Size of the default VM memory, in bytes.
machineSize :: Word
machineSize = 30000

-- | A VM 'Machine' with the default memory available.
emptyMachine :: MachineType
emptyMachine = mkMachine machineSize

-- | Create a new machine with the given memory
mkMachine :: Word -> MachineType
mkMachine n = Machine {memory = GV.replicate (fromIntegral n) 0, pointer = 0}

-- | Command line arguments for the VM evaluator.
data VMOptions = VMOptions
  { vmOptsMemoryBytes :: Word -- ^ Available memory in bytes.
  , vmOptsDumpMemory  :: Bool -- ^ Dump the contents of the memory after executing a program
  , vmOptsProgramPath :: FilePath -- ^ Path to the compiled program
  } deriving (Show)

-- | Default configuration for the VM.
defaultVMOptions :: VMOptions
defaultVMOptions =
  VMOptions
    { vmOptsMemoryBytes = 30000
    , vmOptsDumpMemory = False
    , vmOptsProgramPath = ""
    }

optionsP :: Parser VMOptions
optionsP =
  (\mem dump input ->
     VMOptions
       { vmOptsMemoryBytes = mem
       , vmOptsDumpMemory = dump
       , vmOptsProgramPath = input
       }) <$>
  option
    auto
    (long "memory" <> short 'm' <> metavar "BYTES" <>
     value (vmOptsMemoryBytes defaultVMOptions) <>
     help "Size of the memory [in bytes] used to run the program") <*>
  switch
    (long "dump-memory" <> short 'd' <>
     help "Dump the contents of the memory when the program is finished") <*>
  argument str (metavar "PROGRAM" <> help "Path to the compiled program")

parserInfo :: ParserInfo VMOptions
parserInfo =
  info
    (optionsP <**> helper)
    (fullDesc <> progDesc "Run the compiled Brainfuck program in PROGRAM file" <>
     header "An optimizing Brainfuck compiler and evaluator")

-- | Parse a list of command line arguments
parsePure :: [String] -> ParserResult VMOptions
parsePure = execParserPure defaultPrefs parserInfo

-- | Parse a list of command line arguments printing errors to the stderr
unsafeParse :: [String] -> IO VMOptions
unsafeParse = handleParseResult . parsePure

-- | Parse command line arguments printing errors to the stderr
parse :: IO VMOptions
parse = getArgs >>= unsafeParse
