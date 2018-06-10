{-# LANGUAGE RecordWildCards #-}

module HBF.Compiler
  ( BFP.ParseError
  , module HBF.Compiler
  ) where

import           Control.Monad        (when)
import qualified Data.Binary          as B
import           Data.ByteString.Lazy (ByteString)
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

saveCompilerOutput :: OptimizedProgram -> FilePath -> IO ()
saveCompilerOutput = flip B.encodeFile . instructions

inMemoryCompile ::
     CompilerOptions
  -> Text
  -> Either BFP.ParseError (OptimizedProgram, CompilationSummary)
inMemoryCompile opts code =
  (\p -> (p, summarizeCompilation p)) . optimize opts <$> BFP.parseProgram code

data CompilationSummary = CompilationSummary
  { compNumInstructions :: Int
  } deriving (Show)

summarizeCompilation :: OptimizedProgram -> CompilationSummary
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

optimize :: CompilerOptions -> UnoptimizedProgram -> OptimizedProgram
optimize CompilerOptions {..} p = foldl (flip ($)) base optimizations
  where
    base = toIR p
    opt condition f program =
      if condition
        then f program
        else program
    optimizations = [opt cOptsFusionOptimization fusionOpt]

toIR :: UnoptimizedProgram -> OptimizedProgram
toIR = fmap convert
  where
    convert (Loop l) = OLoop $ convert <$> l
    convert Inc      = IncN 1
    convert Dec      = IncN (-1)
    convert MRight   = MRightN 1
    convert MLeft    = MRightN (-1)
    convert In       = InN 1
    convert Out      = OutN 1

newtype FusedProgram = Fused
  { unfused :: OptimizedProgram
  }

instance Semigroup FusedProgram where
  Fused (Program p1) <> Fused (Program p2) = Fused $ Program $ fuse p1 p2
    where
      fuse :: [OptimizedOp] -> [OptimizedOp] -> [OptimizedOp]
      fuse [] ops           = ops
      fuse ops []           = ops
      fuse [op1] (op2:more) = join op1 op2 ++ more
      fuse (op1:more) ops2  = op1 : fuse more ops2
      join :: OptimizedOp -> OptimizedOp -> [OptimizedOp]
      join (IncN a) (IncN b)       = ifNotZero IncN $ a + b
      join (MRightN a) (MRightN b) = ifNotZero MRightN $ a + b
      join (InN a) (InN b)         = ifNotZero InN $ a + b
      join (OutN a) (OutN b)       = ifNotZero OutN $ a + b
      join a b                     = [a, b]
      ifNotZero f n = [f n | n /= 0]

instance Monoid FusedProgram where
  mempty = Fused mempty
  mappend = (<>)

fusionOpt :: OptimizedProgram -> OptimizedProgram
fusionOpt = unfused . foldMap (Fused . Program . optimizeIn) . instructions
  where
    optimizeIn (OLoop as) = [OLoop inner | not (null inner)]
      where
        inner = instructions $ fusionOpt $ Program as
    optimizeIn other = [other]

load :: ByteString -> OptimizedProgram
load = B.decode

loadFile :: FilePath -> IO OptimizedProgram
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
