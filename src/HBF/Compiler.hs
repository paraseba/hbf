{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module HBF.Compiler
  ( BFP.ParseError
  , module HBF.Compiler
  ) where

import           Control.Monad         (when)
import qualified Data.Binary           as B
import           Data.ByteString.Lazy  (ByteString)
import           Data.Coerce           (coerce)
import           Data.Functor.Identity (Identity)
import           Data.Maybe            (fromMaybe)
import           Data.Semigroup        (Semigroup (..), (<>))
import           Data.Text.Lazy        (Text)
import qualified Data.Text.Lazy.IO     as TIO
import           Data.Tuple            (swap)
import           Options.Applicative
import           System.Environment    (getArgs)
import           System.FilePath       ((-<.>))
import qualified Text.Parsec           as Parsec
import           Text.Parsec.Pos       (initialPos)

import qualified HBF.Parser            as BFP
import           HBF.Types

saveCompilerOutput :: Program Optimized -> FilePath -> IO ()
saveCompilerOutput = flip B.encodeFile . instructions

inMemoryCompile ::
     CompilerOptions
  -> Text
  -> Either BFP.ParseError (Program Optimized, CompilationSummary)
inMemoryCompile opts code =
  (\p -> (p, summarizeCompilation p)) . optimize opts <$> BFP.parseProgram code

data CompilationSummary = CompilationSummary
  { compNumInstructions :: Int
  } deriving (Show)

summarizeCompilation :: Program Optimized -> CompilationSummary
summarizeCompilation = CompilationSummary . length . instructions

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

toIR :: Program Unoptimized -> Program Optimized
toIR = coerce

newtype FusedProgram = Fused
  { unfused :: Program Optimized
  } deriving (Show)

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
      join (MRight a) (MRight b) = ifNotZero MRight $ a + b
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

instance Monoid FusedProgram where
  mempty = Fused mempty
  mappend = (<>)

fusionOpt :: Program Optimized -> Program Optimized
fusionOpt = unfused . foldMap (Fused . Program . optimizeIn) . instructions
  where
    optimizeIn (Loop as) = [Loop inner | not (null inner)]
      where
        inner = instructions $ fusionOpt $ Program as
    optimizeIn other = [other]

liftLoop :: ([Op] -> Maybe [Op]) -> Program o -> Program o
liftLoop f = Program . (>>= g) . instructions
  where
    g :: Op -> [Op]
    g (Loop ops) =
      fromMaybe ((: []) . Loop . instructions . liftLoop f $ Program ops) $
      f ops
    g other = [other]

clearOpt :: Program Optimized -> Program Optimized
clearOpt = liftLoop onLoops
  where
    onLoops :: [Op] -> Maybe [Op]
    onLoops [Inc (-1) 0] = Just [Clear 0]
    onLoops _            = Nothing

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
              (totalOff + off, res ++ [Mul fact (off + totalOff)]) -- todo very inefficient  foldr

scanOpt :: Program Optimized -> Program Optimized
scanOpt = liftLoop onLoops
  where
    onLoops :: [Op] -> Maybe [Op]
    onLoops [MRight 1]    = Just [Scan Up 0]
    onLoops [MRight (-1)] = Just [Scan Down 0]
    onLoops _             = Nothing

offsetInstructionOpt :: Program Optimized -> Program Optimized
offsetInstructionOpt = id -- fixme

load :: ByteString -> Program Optimized
load = B.decode

loadFile :: FilePath -> IO (Program Optimized)
loadFile = B.decodeFile

data CompilerOptions = CompilerOptions
  { cOptsOut                            :: Maybe FilePath
  , cOptsFusionOptimization             :: Bool
  , cOptsClearLoopOptimization          :: Bool
  , cOptsMulOptimization                :: Bool
  , cOptsScanOptimization               :: Bool
  , cOptsOffsetInstructionsOptimization :: Bool
  , cOptsVerbose                        :: Bool
  , cOptsSource                         :: FilePath
  } deriving (Show)

optionsP :: Parser CompilerOptions
optionsP =
  CompilerOptions <$>
  optional
    (option
       str
       (long "output" <> short 'o' <> metavar "OUT" <>
        help "Compiled output path")) <*>
  flag
    True
    False
    (long "disable-fusion-optimization" <>
     help
       "Disable fusion optimization (turn multiple + or > into a single operation)") <*>
  flag
    True
    False
    (long "disable-clear-loop-optimization" <>
     help "Disable clear loop optimization (turn [-] into a single operation)") <*>
  flag
    True
    False
    (long "disable-mul-loop-optimization" <>
     help
       "Disable mul loop optimization (turn [->++>+++<<] into [Mul(1, 2) Mul(2,3)] Clear operations)") <*>
  flag
    True
    False
    (long "disable-scan-loop-optimization" <>
     help "Disable scan loop optimization (turn [>] into ScanR operation)") <*>
  flag
    True
    False
    (long "disable-offset-instructions-optimization" <>
     help
       "Disable offset instructions optimization (turn >>+>->> into Inc 1 2, Inc (-1) 1, MRight 1, MRight 1, MRight 1, MRight 1, MRight 1, operation)") <*>
  switch
    (long "verbose" <> short 'v' <> help "Output more debugging information") <*>
  argument str (metavar "SRC" <> help "Input source code file")

options :: ParserInfo CompilerOptions
options =
  info
    (optionsP <**> helper)
    (fullDesc <> progDesc "Compile Brainfuck code in SRC file" <>
     header "An optimizing Brainfuck compiler and evaluator")

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

parsePure :: [String] -> ParserResult CompilerOptions
parsePure = execParserPure defaultPrefs options

unsafeParse :: [String] -> IO CompilerOptions
unsafeParse = handleParseResult . parsePure

parse :: IO CompilerOptions
parse = getArgs >>= unsafeParse

----------------------- implementation details ----------------------
type ProgramParser a = Parsec.ParsecT [Op] () Identity a

satisfy' :: Show t => (t -> Bool) -> Parsec.ParsecT [t] () Identity t
satisfy' predicate = Parsec.token showTok posFromTok testTok
  where
    showTok t = show t
    posFromTok _ = initialPos ""
    testTok t =
      if predicate t
        then Just t
        else Nothing

mrightP :: ProgramParser MemOffset
mrightP =
  satisfy' isRight <&> \case
    MRight n -> n
    _ -> undefined

mleftP :: ProgramParser MemOffset
mleftP =
  satisfy' isLeft <&> \case
    MRight n -> (negate n)
    _ -> undefined

plusP :: ProgramParser Int
plusP =
  satisfy' isPlus <&> \case
    Inc n 0 -> n
    _ -> undefined

minusP :: ProgramParser Int
minusP =
  satisfy' isMinus <&> \case
    Inc n 0 -> (negate n)
    _ -> undefined

summedP :: Num n => ProgramParser n -> ProgramParser n
summedP = fmap sum . Parsec.many1

isRight :: Op -> Bool
isRight (MRight n)
  | n > 0 = True
isRight _ = False

isLeft :: Op -> Bool
isLeft (MRight n)
  | n < 0 = True
isLeft _ = False

isPlus :: Op -> Bool
isPlus (Inc n 0)
  | n > 0 = True
isPlus _ = False

isMinus :: Op -> Bool
isMinus (Inc n 0)
  | n < 0 = True
isMinus _ = False

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
