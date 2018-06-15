{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase     #-}

module HBF.Compiler
  ( BFP.ParseError
  , module HBF.Compiler
  ) where

import           Control.Monad        (when)
import qualified Data.Binary          as B
import           Data.ByteString.Lazy (ByteString)
import           Data.Coerce          (coerce)
import           Data.Maybe           (fromMaybe)
import           Data.Semigroup       ((<>), Semigroup(..))
import           Data.Text.Lazy       (Text)
import qualified Data.Text.Lazy.IO    as TIO
import           Options.Applicative
import           System.Environment   (getArgs)
import           System.FilePath      ((-<.>))

import Data.Functor.Identity (Identity)
import    qualified       Text.Parsec as Parsec
import    Text.Parsec.Pos (initialPos)

import qualified HBF.Parser           as BFP
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
      , opt cOptsFusionOptimization fusionOpt]

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
      join (Inc a) (Inc b)       = ifNotZero Inc $ a + b
      join (MRight a) (MRight b) = ifNotZero MRight $ a + b
      join (In a) (In b)         = ifNotZero In $ a + b
      join (Out a) (Out b)       = ifNotZero Out $ a + b
      join Clear Clear           = [Clear]
      join ScanR ScanR           = [ScanR]
      join ScanL ScanL           = [ScanL]
      join a b                   = [a, b]
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
    g (Loop ops) = fromMaybe ((:[]). Loop . instructions . liftLoop f $ Program ops) $ f ops
    g other = [other]

clearOpt :: Program Optimized -> Program Optimized
clearOpt = liftLoop onLoops
  where
    onLoops :: [Op] -> Maybe [Op]
    onLoops [Inc (-1)] = Just [Clear]
    onLoops _ = Nothing

mulOpt :: Program Optimized -> Program Optimized
mulOpt = liftLoop onLoops
  where
    onLoops :: [Op] -> Maybe [Op]
    onLoops ops =
      makeOp <$> eitherToMaybe (Parsec.parse mulP "" ops)
      where
        makeOp :: [(MulOffset, MulFactor)] -> [Op]
        makeOp = (++ [Clear]). snd . foldl it (0, [])
          where
            it (totalOff, res) (off,fact) =
              (totalOff + off, res ++ [Mul (off + totalOff) fact]) -- todo very inefficient  foldr

scanOpt :: Program Optimized -> Program Optimized
scanOpt = liftLoop onLoops
  where
    onLoops :: [Op] -> Maybe [Op]
    onLoops [MRight 1] = Just [ ScanR ]
    onLoops [MRight (-1)] = Just [ ScanL ]
    onLoops _ = Nothing

load :: ByteString -> Program Optimized
load = B.decode

loadFile :: FilePath -> IO (Program Optimized)
loadFile = B.decodeFile

data CompilerOptions = CompilerOptions
  { cOptsOut                :: Maybe FilePath
  , cOptsFusionOptimization :: Bool
  , cOptsClearLoopOptimization          :: Bool
  , cOptsMulOptimization          :: Bool
  , cOptsScanOptimization          :: Bool
  , cOptsVerbose            :: Bool
  , cOptsSource             :: FilePath
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
     help "Disable mul loop optimization (turn [->++>+++<<] into [Mul(1, 2) Mul(2,3)] Clear operations)") <*>
  flag
    True
    False
    (long "disable-scan-loop-optimization" <>
     help "Disable scan loop optimization (turn [>] into ScanR operation)") <*>
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

type ProgramParser  a = Parsec.ParsecT [Op] () Identity a

satisfy' :: Show t => (t -> Bool) -> Parsec.ParsecT [t] () Identity t
satisfy' predicate
   = Parsec.token showTok posFromTok testTok
   where
     showTok t       = show t
     posFromTok _  = initialPos ""
     testTok t     = if predicate t then Just t else Nothing

mrightP  :: ProgramParser Int
mrightP =  satisfy' isRight <&> \case
  MRight n -> n
  _ -> undefined

mleftP :: ProgramParser Int
mleftP =  satisfy' isLeft <&> \case
  MRight n -> (negate n)
  _ -> undefined

plusP :: ProgramParser Int
plusP = satisfy' isPlus <&> \case
  Inc n -> n
  _ -> undefined

minusP :: ProgramParser Int
minusP = satisfy' isMinus <&> \case
  Inc n -> (negate n)
  _ -> undefined


summedP :: ProgramParser  Int -> ProgramParser Int
summedP = fmap sum . Parsec.many1

isRight :: Op -> Bool
isRight (MRight n) | n > 0 = True
isRight _ = False

isLeft :: Op -> Bool
isLeft (MRight n) | n < 0 = True
isLeft _ = False

isPlus :: Op -> Bool
isPlus (Inc n) | n > 0 = True
isPlus _ = False

isMinus :: Op -> Bool
isMinus (Inc n) | n < 0 = True
isMinus _ = False


mulP :: ProgramParser [(MulOffset, MulFactor)]
mulP = do
  _ <- minusP
  copies <- Parsec.many1 shiftFactorP
  let totalShift = sum $ map fst copies
  back <- summedP mleftP
  Parsec.eof
  if back == totalShift
  then return $ coerce copies
  else Parsec.unexpected "number of left returns to close the loop"
  where
    shiftFactorP = (,) <$> summedP mrightP <*> summedP plusP
