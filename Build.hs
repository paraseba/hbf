{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE TypeFamilies   #-}

import           Control.DeepSeq
import           Data.Binary
import           Data.Char         (toLower)
import           Data.Hashable
import           Data.List
import           Data.Maybe        (fromMaybe, mapMaybe)
import           Data.Typeable
import           Development.Shake
import           GHC.Generics

data ConfigureType
  = Base
  | Tests
  | Benchmarks
  | Coverage
  deriving (Show, Eq, Ord, Generic, Typeable, Hashable, Binary, NFData)

newtype ConfigureMapQ =
  ConfigureMapQ ()
  deriving (Show, Generic, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult ConfigureMapQ = [(ConfigureType, Bool)]

newtype ConfigureStatusQ =
  ConfigureStatusQ ConfigureType
  deriving (Show, Generic, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult ConfigureStatusQ = Bool

charToConfigureStatus :: Char -> Maybe ConfigureType
charToConfigureStatus c =
  case toLower c of
    't' -> Just Tests
    'b' -> Just Benchmarks
    'c' -> Just Coverage
    _   -> Nothing

defaultConfigure :: [ConfigureType]
defaultConfigure = [Base]

matchConfigureRule :: String -> Maybe [ConfigureType]
matchConfigureRule s =
  if "configure*" ?== s
    then if null suffix
           then Just defaultConfigure
           else if length suffix > 3
                  then Nothing
                  else Just $ mapMaybe charToConfigureStatus suffix
    else Nothing
  where
    suffix = drop (length "configure") s

configure :: [ConfigureType] -> [ConfigureType] -> Action ()
configure wanted current = do
  putNormal $ "Configuring for: " ++ show swanted
  if swanted == scurrent
    then putNormal "Nothing to configure"
    else do
      putNormal $ "Changing current state: " ++ show scurrent
      cmd_ $ nixrun $ "cabal configure " ++ unwords (configureFlags swanted)
  where
    swanted = sort wanted
    scurrent = sort current
    configureFlags :: [ConfigureType] -> [String]
    configureFlags = map confName
    confName Tests      = "--enable-tests"
    confName Benchmarks = "--enable-benchmarks"
    confName Coverage   = "--enable-coverage"

ensureConfigure :: [ConfigureType] -> [ConfigureType] -> Action ()
ensureConfigure wanted current = do
  if null confneeded
    then putNormal "Nothing to configure"
    else configure (current ++ confneeded) current
  where
    confneeded = wanted \\ current

nix :: [String] -> [String]
nix args = ["nix-shell", "--attr", "hbf.env", "release.nix"] ++ args

nixrun :: String -> [String]
nixrun c = nix ["--run", c]

cabalFlagsFile :: FilePath
cabalFlagsFile = "./dist/cabal-config-flags"

main :: IO ()
main =
  shakeArgs shakeOptions {shakeFiles = "_build"} $
    -- fake write to file if nothing configured
   do
    getConfigure <-
      addOracle $ \ConfigureMapQ {} -> do
        configured <- doesFileExist cabalFlagsFile
        if configured
          then do
            flags <- readFile' cabalFlagsFile
            return
              [ (Tests, "--enable-tests" `isInfixOf` flags)
              , (Benchmarks, "--enable-benchmarks" `isInfixOf` flags)
              , (Coverage, "--enable-coverage" `isInfixOf` flags)
              ]
          else return []
    getConfigureStatus <-
      addOracle $ \(ConfigureStatusQ ctype) ->
        fromMaybe False . lookup ctype <$> getConfigure (ConfigureMapQ ())
    "default.nix" %> \out -> do
      need ["hbf.cabal"]
      Stdout stdout <- cmd "cabal2nix ."
      writeFileChanged out stdout
    phony "clean" $ do
      putNormal "Cabal cleaning"
      cmd_ "cabal clean"
    let status = getConfigureStatus . ConfigureStatusQ
        currentStatuses = do
          t <- status Tests
          b <- status Benchmarks
          c <- status Coverage
          return $ [Tests | t] ++ [Benchmarks | b] ++ [Coverage | c]
    phony "watchtest" $ do
      putNormal "Running ghcid for tests"
      cmd_ $ nixrun "ghcid -W -T Main.main"
    phony "watchbuild" $ do
      putNormal "Running ghcid for build"
      cmd_ $ nixrun "ghcid"
    phony "style" $ do
      sources <- getDirectoryFiles "." ["//*.hs", "//*.lhs"]
      cmd_ "hindent" sources
      cmd_ "stylish-haskell" $ "-i" : sources
    phony "confstate" $ do
      let enabled True  = " enabled"
          enabled False = " disabled"
          write t = status t >>= putNormal . (show t ++) . enabled
      write Tests
      write Benchmarks
      write Coverage
    phony "debug" $ do
      s <- currentStatuses
      putNormal $ show s
    -- configure, configuretbc, configuret, etc.
    phonys $
      fmap
        (\wanted -> do
           need ["default.nix", "release.nix", "hbf.cabal"]
           current <- currentStatuses
           configure wanted current) .
      matchConfigureRule
    let exetest = "./dist/build/test/test"
        exebench = "./dist/build/evalbench/evalbench"
        execompiler = "./dist/build/hbfc/hbfc"
        exevm = "./dist/build/hbf/hbf"
    phony "test" $ do
      need [exetest]
      cmd_ exetest
    exetest %> \_ -> do
      current <- currentStatuses
      ensureConfigure [Tests] current
      cmd_ "cabal" "build" "test"
    phony "bench" $ do
      need ["public/.dummy"]
      need [exebench]
      cmd_ exebench ["-o", "public/bench.html"]
    exebench %> \_ -> do
      current <- currentStatuses
      ensureConfigure [Benchmarks] current
      cmd_ "cabal" "build" "bench:evalbench"
    phony "coverage" $ do
      need ["clean", "public/.dummy"]
      oldstatus <- currentStatuses
      configure [Coverage, Tests] []
      cmd_ "cabal" "test"
      cmd_ "cp" "-r" "dist/hpc/vanilla/html/test" "public/coverage"
      need ["clean"]
      configure oldstatus [Coverage, Tests]
    phony "buildall" $ do
      current <- currentStatuses
      ensureConfigure [Tests, Benchmarks] current
      cmd_ "cabal" "build"
    phony "build" $ need ["buildall"]
    execompiler %> \_ -> do
      current <- currentStatuses
      ensureConfigure [] current
      cmd_ "cabal" "build" "exe:hbfc"
    exevm %> \_ -> do
      current <- currentStatuses
      ensureConfigure [] current
      cmd_ "cabal" "build" "exe:hbf"
    phony "compiler" $ need [execompiler]
    phony "vm" $ need [exevm]
    "public/.dummy" %> \out -> do
      cmd_ "mkdir" "-p" "./public"
      cmd_ "touch" out
