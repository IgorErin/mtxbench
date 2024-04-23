module Bench (runMany) where

import Criterion.Main (Benchmark, bench, nfIO, runMode)
import Criterion.Main.Options (Mode(..), MatchType(..), defaultConfig)

import System.Process.Typed ( runProcess_, proc, setStdout, closed )
import Criterion.Types (Config(resamples))

runFile ::  FilePath -> IO ()
runFile fp = runProcess_ $
    setStdout closed $
    proc fp []

run :: FilePath -> Benchmark
run fp = bench fp $ nfIO $ runFile fp

config :: Config
config = defaultConfig { resamples = 20 }

mode :: Mode
mode = Run config Glob []

runMany :: [FilePath] ->  IO ()
runMany fs = runMode mode $ run <$> fs