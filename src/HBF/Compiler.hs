{-# LANGUAGE RecordWildCards #-}

module HBF.Compiler
  ( BFP.ParseError
  , module HBF.Compiler
  ) where

import           Control.Monad        (when)
import qualified Data.Binary          as B
import           Data.ByteString.Lazy (ByteString)
import           Data.Coerce          (coerce)
import           Data.Maybe           (fromMaybe)
import           Data.Semigroup       ((<>))
import           Data.Semigroup       (Semigroup (..))
import           Data.Text.Lazy       (Text)
import qualified Data.Text.Lazy.IO    as TIO
import           Options.Applicative
import           System.Environment   (getArgs)
import           System.FilePath      ((-<.>))

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
    opt condition f program =
      if condition
        then f program
        else program
    optimizations = [opt cOptsFusionOptimization fusionOpt]

toIR :: Program Unoptimized -> Program Optimized
toIR = coerce

newtype FusedProgram = Fused
  { unfused :: Program Optimized
  }

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

load :: ByteString -> Program Optimized
load = B.decode

loadFile :: FilePath -> IO (Program Optimized)
loadFile = B.decodeFile

data CompilerOptions = CompilerOptions
  { cOptsOut                :: Maybe FilePath
  , cOptsFusionOptimization :: Bool
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
       "Disable fusion optimization (turn multiple + or > in a single operation)") <*>
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
    , cOptsVerbose = False
    , cOptsSource = ""
    }

noOptimizationCompilerOptions :: CompilerOptions
noOptimizationCompilerOptions =
  CompilerOptions
    { cOptsOut = Nothing
    , cOptsFusionOptimization = False
    , cOptsVerbose = False
    , cOptsSource = ""
    }

parsePure :: [String] -> ParserResult CompilerOptions
parsePure = execParserPure defaultPrefs options

unsafeParse :: [String] -> IO CompilerOptions
unsafeParse = handleParseResult . parsePure

parse :: IO CompilerOptions
parse = getArgs >>= unsafeParse
