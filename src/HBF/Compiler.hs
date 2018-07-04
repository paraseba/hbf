{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Description : Brainfuck compilation to IR
Copyright   : (c) Sebastian Galkin, 2018
License     : GPL-3

In this module we:

    - Convert 'Text' into a Brainfuck intermediate representation (IR) consisting of lists of 'Op's.
    - Provide optimization rules to speed up IR execution.
    - Parse compiler command line options
-}
module HBF.Compiler
  ( module HBF.Compiler
  -- * Reexport from "BFP.Parser"
  , BFP.ParseError
  ) where

import           Control.Monad             (when)
import           Control.Monad.Trans.State.Strict (State, execState, get, modify, put)
import qualified Data.Binary               as B
import           Data.ByteString.Lazy      (ByteString)
import           Data.Coerce               (coerce)
import           Data.Foldable             (traverse_)
import           Data.Functor.Identity     (Identity)
import           Data.Maybe                (fromMaybe)
import           Data.Semigroup            (Semigroup (..), (<>))
import           Data.Text.Lazy            (Text)
import qualified Data.Text.Lazy.IO         as TIO
import           Data.Tuple                (swap)
import           Options.Applicative       (Parser, ParserInfo, ParserResult,
                                            argument, defaultPrefs,
                                            execParserPure, fullDesc,
                                            handleParseResult, header, help,
                                            helper, info, long, metavar, option,
                                            optional, progDesc, short, str,
                                            switch, (<**>))
import           System.Environment        (getArgs)
import           System.FilePath           ((-<.>))
import qualified Text.Parsec               as Parsec
import           Text.Parsec.Pos           (initialPos)

import qualified HBF.Parser                as BFP
import           HBF.Types

-- * Compilation
-- | Encode the compiled file into the given path.
saveCompilerOutput :: Program Optimized -> FilePath -> IO ()
saveCompilerOutput = flip B.encodeFile . instructions

-- | Use the given 'CompilerOptions' to parse, compile and optimize the text representation of a
-- Brainfuck program into the IR. 'cOptsSource' and 'cOptsOut' in the compiler options are ignored.
inMemoryCompile ::
     CompilerOptions
  -> Text
  -> Either BFP.ParseError (Program Optimized, CompilationSummary)
inMemoryCompile opts code =
  (\p -> (p, summarizeCompilation p)) . optimize opts <$> BFP.parseProgram code

-- | Compilation summary for the user. It contains overview information and
-- statistics about the compilation result.
newtype CompilationSummary = CompilationSummary
  { compNumInstructions :: Int
  } deriving (Show)

-- | Summarize a compiled program creating the 'CompilationSummary'
summarizeCompilation :: Program Optimized -> CompilationSummary
summarizeCompilation = CompilationSummary . length . instructions

-- | Use 'CompilerOptions' to read, compile, optimize, and save a program from/to the filesystem.
-- Input and output files are provided by 'cOptsSource' and 'cOptsOut'.
compile :: CompilerOptions -> IO (Either BFP.ParseError CompilationSummary)
compile opts@CompilerOptions {..} = do
  when cOptsVerbose $ do
    putStrLn "Compiler options:"
    print opts
  compileResult <- inMemoryCompile opts <$> TIO.readFile cOptsSource
  either
    (return . Left)
    (\p -> save p >> (return . Right . snd) p)
    compileResult
  where
    outPath = fromMaybe (cOptsSource -<.> "bfc") cOptsOut
    save (program, _) = saveCompilerOutput program outPath

-- | Apply optimizations to the 'Unoptimized' program turning. The optimizations that
-- will be available are the ones specified by the 'CompilerOptions' given.
optimize :: CompilerOptions -> Program Unoptimized -> Program Optimized
optimize CompilerOptions {..} p = foldl (flip ($)) base optimizations
  where
    base = toIR p
    opt condition f =
      if condition
        then f
        else id
    optimizations =
      [ opt cOptsClearLoopOptimization clearOpt
      , opt cOptsMulOptimization mulOpt
      , opt cOptsScanOptimization scanOpt
      , opt cOptsOffsetInstructionsOptimization offsetInstructionOpt
      , opt cOptsFusionOptimization fusionOpt
      ]

-- | Given a parsed program, turn it into an optimized one, but with the null optimization.
-- Effectively this is only a type change.
toIR :: Program Unoptimized -> Program Optimized
toIR = coerce

-- * Optimization
-- | Helper type to apply the Fuse optimization using a 'Monoid'.
newtype FusedProgram = Fused
  { unfused :: Program Optimized
  } deriving (Show)

-- | This 'Semigroup' for 'FusedProgram' does all the fusion optimization work.
-- When two contiguous optimizations can be fused into one, '<>' will reduce the
-- size of the list in the 'FusedProgram'.
--
-- Examples of fusable operations:
--
--    - (Inc a offset) (Inc b offset) -> (Inc (a+b) offset)
--    - (Move 3 offset) (Move (-3) offset) -> NoOp
--    - (Clear offset) (Clear offset) -> Clear offset
--    - (Scan Up offset) (Scan _ offset') -> Scan Up offset
instance Semigroup FusedProgram where
  Fused (Program p1) <> Fused (Program p2) = Fused $ Program $ fuse p1 p2
    where
      fuse :: [Op] -> [Op] -> [Op]
      fuse [] ops           = ops
      fuse ops []           = ops
      fuse [op1] (op2:more) = join op1 op2 ++ more
      fuse (op1:more) ops2  = op1 : fuse more ops2
      join :: Op -> Op -> [Op]
      join (Inc a n) (Inc b m)
        | n == m = ifNotZero (flip Inc n) $ a + b
      join (Move a) (Move b) = ifNotZero Move $ a + b
      join (In a n) (In b m)
        | n == m = ifNotZero (flip In n) $ a + b
      join (Out a n) (Out b m)
        | n == m = ifNotZero (flip Out n) $ a + b
      join (Clear n) (Clear m)
        | n == m = [Clear n]
      -- once a scan is found, another one won't move the pointer
      join (Scan Up o1) (Scan _ o2)
        | o1 == o2 = [Scan Up o1]
      join (Scan Down o1) (Scan _ o2)
        | o1 == o2 = [Scan Down o1]
      join a b = [a, b]
      ifNotZero f n = [f n | n /= 0]

-- | Use the 'Semigroup' instance and an empty program as 'mempty'.
instance Monoid FusedProgram where
  mempty = Fused mempty
  mappend = (<>)

-- | Apply the fusion optimization using the 'FusedProgram' 'Monoid' instance.
--
-- The fusion optimization consist of turning multiple instructions into one. For example
-- if the original Brainfuck code contains '++++', this would be parsed as
--
-- @
--'Program' ['Inc' 1 0, 'Inc' 1 0, 'Inc' 1 0, 'Inc' 1 0]
-- @
--
-- but it would be fused to a single IR instruction: @Inc 4 0@.
--
-- >>> fusionOpt $ Program [Inc 1 0, Inc 1 0, Inc 1 0, Inc 1 0]
-- [Inc 4 0]
--
-- Similarly, other instructions,
-- like 'Move', 'In', 'Out', 'Clear' and 'Scan' can be fused as long as the offset at which they
-- must be applied is the same.
--
-- Non fusable operation remain unchanged:
--
-- >>> fusionOpt $ Program [Inc 1 0, Inc 1 1]
-- [Inc 1 0,Inc 1 1]
fusionOpt :: Program Optimized -> Program Optimized
fusionOpt = unfused . foldMap (Fused . Program . optimizeIn) . instructions
  where
    optimizeIn (Loop as) = [Loop inner | not (null inner)]
      where
        inner = instructions $ fusionOpt $ Program as
    optimizeIn other = [other]

-- | Helper function used to implement optimizations
-- Iterate over all 'Program' instructions searching for 'Loop's. For each 'Loop'
-- apply 'f'. If 'f' returns a list of new operations, replace the original loop with
-- the new instructions. If 'f' returns 'Nothing', process recursively the loop instructions.
liftLoop :: ([Op] -> Maybe [Op]) -> Program o -> Program o
liftLoop f = Program . (>>= g) . instructions
  where
    g :: Op -> [Op]
    g (Loop ops) =
      fromMaybe ((: []) . Loop . instructions . liftLoop f $ Program ops) $
      f ops
    g other = [other]

-- | Basic optimization that turns the loop @[-]@ into a single instruction 'Clear'.
-- Useful because clearing a memory position is a pretty common operation in Brainfuck and
-- very expensive if treated as a loop.
--
-- >>> :set -XOverloadedStrings
-- >>> Right (res, _) = inMemoryCompile defaultCompilerOptions "[-]"
-- >>> res
-- [Clear 0]
clearOpt :: Program Optimized -> Program Optimized
clearOpt = liftLoop onLoops
  where
    onLoops :: [Op] -> Maybe [Op]
    onLoops [Inc (-1) 0] = Just [Clear 0]
    onLoops _            = Nothing

-- | Copy and multiply optimization. A very common usage of loops is to copy the value of a memory
-- position to a different: @[->>+<<]@ this will move the contents of the current memory position
-- to places to the right, also clearing the original position to zero. If we change the number of @+@
-- operations we get multiplication, if we have several groups of @++..@ operations we get multiple copies.
-- In the general case, for example:
--
-- >>> :set -XOverloadedStrings
-- >>> Right (res, _) = inMemoryCompile defaultCompilerOptions "[->+>++>++++<<<]"
-- >>> res
-- [Mul 1 0 1,Mul 2 0 2,Mul 4 0 3,Clear 0]
--
-- The original Brainfuck copies the current position one place to the right, doubles
-- the current position two places to the right, and quadruples the current position three places to the right;
-- finally zeroing the current position. With the mul optimization in this function, all that loop would be
-- replaced by 4 instructions.
mulOpt :: Program Optimized -> Program Optimized
mulOpt = liftLoop onLoops
  where
    onLoops :: [Op] -> Maybe [Op]
    onLoops ops = makeOp <$> eitherToMaybe (Parsec.parse mulP "" ops)
      where
        makeOp :: [(MulFactor, MemOffset)] -> [Op]
        makeOp = (++ [Clear 0]) . snd . foldl it (0, [])
          where
            it (totalOff, res) (fact, off) =
              (totalOff + off, res ++ [Mul fact 0 (off + totalOff)]) -- todo very inefficient  foldr

-- | Implement the scan optimization. Another common operation in Brainfuck is to search for the first zero
-- in the neighboring memory, either to the right or to the left @[>]@ or @[<]@. These loops can be replaced
-- for a more optimal search, represented as a single @'Scan' 'Up'@ or @'Scan' 'Down'@ instruction.
--
-- >>> scanOpt $ Program [Loop [Move 1]]
-- [Scan Up 0]
scanOpt :: Program Optimized -> Program Optimized
scanOpt = liftLoop onLoops
  where
    onLoops :: [Op] -> Maybe [Op]
    onLoops [Move 1]    = Just [Scan Up 0]
    onLoops [Move (-1)] = Just [Scan Down 0]
    onLoops _           = Nothing

-- | Helper datastructure to implement a stateful transformation in 'offsetInstructionOpt'.
data OffsetState = OffSt
  { stOptimized :: [Op] -- ^ The optimized program so far
  , stBatch     :: [Op] -- ^ The current batch of instructions being optimized (between loops)
  , stOffset    :: MemOffset -- ^ The current offset since the last loop
  } deriving (Show)

-- | Start state for 'offsetInstructionOpt'.
emptyState :: OffsetState
emptyState = OffSt [] [] 0

-- | Implement the offset instruction optimization. This is probably the most complex
-- optimization implemented in the library.
--
-- In streams of instructions between loops, there is no need to keep updating the current position
-- if we can keep track of where the different operations should be applied. This is a trade-off
-- of time (not updating the pointer) by space (keeping track of the offset in every operation).
-- For example the following unoptimized code
--
--
-- >>> offsetInstructionOpt  $ Program [Loop [], Move 1, Inc 1 0, Move 2, Clear 0, Mul 2 0 1, Loop []]
-- [Loop [],Inc 1 1,Clear 3,Mul 2 3 1,Move 3,Loop []]
--
-- And the optimization eliminated one 'Move' instruction. In general, for larger programs the gain
-- will be more noticeable.
--
-- An important detail to take into account is that 'Scan' operations break the stream of operations
-- that can be optimized together, and turn the accumulated offset back to zero:
--
-- >>> offsetInstructionOpt  $ Program [Loop [], Move 1, Inc 1 0, Scan Up 0, Inc 0 2, Loop []]
-- [Loop [],Inc 1 1,Scan Up 1,Inc 0 2,Loop []]
offsetInstructionOpt :: Program Optimized -> Program Optimized
offsetInstructionOpt -- We implement this as a stateful computation for code clarity
 =
  Program .
  stOptimized .
  (`execState` emptyState) .
  (*> finishLastBatch) . traverse_ processOp . instructions
  where
    processOp :: Op -> State OffsetState ()
    processOp (Loop l) = do
      let newLoop = Loop (instructions $ offsetInstructionOpt (Program l))
      finishBatch
      modify $ \s@OffSt {..} -> s {stOptimized = newLoop : stOptimized}
    processOp (Move n) = get >>= \s -> put s {stOffset = stOffset s + n}
    processOp (Inc n off) = add off (Inc n)
    processOp (In n off) = add off (In n)
    processOp (Out n off) = add off (Out n)
    processOp (Clear off) = add off Clear
    processOp (Mul factor from to) = add from (\o -> Mul factor o to)
    processOp (Scan d off) = do
      OffSt {..} <- get
      put
        OffSt
          { stOffset = 0
          , stOptimized = stOptimized
          , stBatch = Scan d (off + stOffset) : stBatch
          }
    add :: MemOffset -> (MemOffset -> Op) -> State OffsetState ()
    add off op =
      get >>= \s@OffSt {..} -> put s {stBatch = op (off + stOffset) : stBatch}
    finishBatch :: State OffsetState ()
    finishBatch = do
      s@OffSt {..} <- get
      let batch =
            if stOffset /= 0
              then Move stOffset : stBatch
              else stBatch
      put s {stBatch = [], stOffset = 0, stOptimized = batch ++ stOptimized}
    finishLastBatch :: State OffsetState ()
    finishLastBatch = do
      finishBatch
      modify $ \s@OffSt {..} -> s {stOptimized = reverse stOptimized}

-- * Loading Compiled Code
-- | Load a compiled program from 'saveCompilerOutput' output.
load :: ByteString -> Program Optimized
load = B.decode

-- | Load a compiled program saved with 'saveCompilerOutput'.
loadFile :: FilePath -> IO (Program Optimized)
loadFile = B.decodeFile

-- * Compiler Flags
-- | Command line flags to the Brainfuck compiler
data CompilerOptions = CompilerOptions
  { cOptsOut                            :: Maybe FilePath -- ^ Where to put the compiled output, if 'Nothing' use the input basename with bfc extension
  , cOptsFusionOptimization             :: Bool -- ^ Enable fusion optimization
  , cOptsClearLoopOptimization          :: Bool -- ^ Enable clear loop optimization
  , cOptsMulOptimization                :: Bool -- ^ Enable mul loop optimization
  , cOptsScanOptimization               :: Bool -- ^ Enable scan loop optimization
  , cOptsOffsetInstructionsOptimization :: Bool -- ^ Enable offset instructions optimization
  , cOptsVerbose                        :: Bool -- ^ Output more debugging information
  , cOptsSource                         :: FilePath -- ^ Input source to the compiler, this should be Brainfuck code
  } deriving (Show)

optionsP :: Parser CompilerOptions
optionsP =
  (\output disableAll fusion clear mul scan offset verbose source ->
     CompilerOptions
       { cOptsOut = output
       , cOptsFusionOptimization = not disableAll || fusion
       , cOptsClearLoopOptimization = not disableAll || clear
       , cOptsMulOptimization = not disableAll || mul
       , cOptsScanOptimization = not disableAll || scan
       , cOptsOffsetInstructionsOptimization = not disableAll || offset
       , cOptsVerbose = verbose
       , cOptsSource = source
       }) <$>
  optional
    (option
       str
       (long "output" <> short 'o' <> metavar "OUT" <>
        help "Compiled output path")) <*>
  switch
    (long "disable-all-optimizations" <> short 'd' <>
     help "Disable all optimizations") <*>
  switch
    (long "fusion" <>
     help
       "Reenable fusion optimization (turn multiple + or > into a single operation)") <*>
  switch
    (long "clear" <>
     help "Reenable clear loop optimization (turn [-] into a single operation)") <*>
  switch
    (long "mul" <>
     help
       "Reenable mul loop optimization (turn [->++>+++<<] into [Mul(1, 2) Mul(2,3)] Clear operations)") <*>
  switch
    (long "scan" <>
     help "Reenable scan loop optimization (turn [>] into ScanR operation)") <*>
  switch
    (long "offset" <>
     help
       "Reenable offset instructions optimization (turn >>+>->> into Inc 1 2, Inc (-1) 1, Move 1, Move 1, Move 1, Move 1, Move 1, operation)") <*>
  switch
    (long "verbose" <> short 'v' <> help "Output more debugging information") <*>
  argument str (metavar "SRC" <> help "Input source code file")

options :: ParserInfo CompilerOptions
options =
  info
    (optionsP <**> helper)
    (fullDesc <> progDesc "Compile Brainfuck code in SRC file" <>
     header "An optimizing Brainfuck compiler and evaluator")

-- | Default compiler options: all optimizations, not verbose, no input or output files.
defaultCompilerOptions :: CompilerOptions
defaultCompilerOptions =
  CompilerOptions
    { cOptsOut = Nothing
    , cOptsFusionOptimization = True
    , cOptsClearLoopOptimization = True
    , cOptsMulOptimization = True
    , cOptsScanOptimization = True
    , cOptsOffsetInstructionsOptimization = True
    , cOptsVerbose = False
    , cOptsSource = ""
    }

-- | Compiler options: all optimizations off.
noOptimizationCompilerOptions :: CompilerOptions
noOptimizationCompilerOptions =
  CompilerOptions
    { cOptsOut = Nothing
    , cOptsFusionOptimization = False
    , cOptsClearLoopOptimization = False
    , cOptsMulOptimization = False
    , cOptsScanOptimization = False
    , cOptsOffsetInstructionsOptimization = False
    , cOptsVerbose = False
    , cOptsSource = ""
    }

-- | Parse a list of command line arguments
parsePure :: [String] -> ParserResult CompilerOptions
parsePure = execParserPure defaultPrefs options

-- | Parse a list of command line arguments printing errors to the stderr
unsafeParse :: [String] -> IO CompilerOptions
unsafeParse = handleParseResult . parsePure

-- | Parse command line arguments printing errors to the stderr
parse :: IO CompilerOptions
parse = getArgs >>= unsafeParse

----------------------- implementation details ----------------------
-- * Implementation Detail: Parsing Lists of Instructions
-- | This parser is used to implement the mul optimization. See 'mulOpt'.
type ProgramParser a = Parsec.ParsecT [Op] () Identity a

-- | Parse successfully if the token satisfies the predicate.
satisfy' :: Show t => (t -> Bool) -> Parsec.ParsecT [t] () Identity t
satisfy' predicate = Parsec.token showTok posFromTok testTok
  where
    showTok t = show t
    posFromTok _ = initialPos ""
    testTok t =
      if predicate t
        then Just t
        else Nothing

-- | Parse movement to the right (\>), returning the offset value.
--
-- >>> Parsec.parse mrightP "" [Move 3]
-- Right 3
--
-- >>> Data.Either.isLeft $ Parsec.parse mrightP "" [Move (-1)]
-- True
mrightP :: ProgramParser MemOffset
mrightP =
  satisfy' isRight <&> \case
    Move n -> n
    _ -> undefined

-- | Parsemovement to the left (\<), returning the offset value.
--
-- >>> Parsec.parse mleftP "" [Move (-3)]
-- Right 3
--
-- >>> Data.Either.isLeft $ Parsec.parse mleftP "" [Move 1]
-- True
mleftP :: ProgramParser MemOffset
mleftP =
  satisfy' isLeft <&> \case
    Move n -> (negate n)
    _ -> undefined

-- | Parse increment, returning total increment.
--
-- >>> Parsec.parse plusP "" [Inc 3 0]
-- Right 3
--
-- >>> Data.Either.isLeft $ Parsec.parse plusP "" [Inc (-2) 0]
-- True
plusP :: ProgramParser Int
plusP =
  satisfy' isPlus <&> \case
    Inc n 0 -> n
    _ -> undefined

-- | Parse decrement, returning total decrement.
--
-- >>> Parsec.parse minusP "" [Inc (-3) 0]
-- Right 3
--
-- >>> Data.Either.isLeft $ Parsec.parse minusP "" [Inc 2 0]
-- True
minusP :: ProgramParser Int
minusP =
  satisfy' isMinus <&> \case
    Inc n 0 -> (negate n)
    _ -> undefined

-- | Sum the result of a parser applied repeatedly
--
-- >>> Parsec.parse (summedP plusP) "" [Inc 3 0, Inc 1 0, Inc (-4) 0]
-- Right 4
summedP :: Num n => ProgramParser n -> ProgramParser n
summedP = fmap sum . Parsec.many1

-- | Full multiple copy/multiply operation parser. Returns the set of factors and relative, incremental offsets.
--
-- >>> Parsec.parse mulP "" [Inc (-1) 0, Move 1, Inc 2 0, Move 3, Inc 1 0, Move (-4)]
-- Right [(2,1),(1,3)]
mulP :: ProgramParser [(MulFactor, MemOffset)]
mulP = do
  _ <- minusP
  copies <- Parsec.many1 shiftFactorP
  let totalShift = sum $ map fst copies
  back <- summedP mleftP
  Parsec.eof
  if back == coerce totalShift
    then return (fmap swap copies)
    else Parsec.unexpected "number of left returns to close the loop"
  where
    shiftFactorP = (,) <$> summedP mrightP <*> fmap MulFactor (summedP plusP)

-- | Is the instruction a right movement?
isRight :: Op -> Bool
isRight (Move n)
  | n > 0 = True
isRight _ = False

-- | Is the instruction a left movement?
isLeft :: Op -> Bool
isLeft (Move n)
  | n < 0 = True
isLeft _ = False

-- | Is the instruction an increment?
isPlus :: Op -> Bool
isPlus (Inc n 0)
  | n > 0 = True
isPlus _ = False

-- | Is the instruction a decrement?
isMinus :: Op -> Bool
isMinus (Inc n 0)
  | n < 0 = True
isMinus _ = False
