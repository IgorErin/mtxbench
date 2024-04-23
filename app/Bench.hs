module Bench (runMany) where

import Criterion.Main (Benchmark, bench, nfIO, defaultMain)

import System.Process.Typed (runProcess_, proc)

runFile :: FilePath -> IO ()
runFile fp = runProcess_ $ proc fp []

run :: FilePath -> Benchmark
run fp = bench fp $ nfIO $ runFile fp

runMany :: [FilePath] ->  IO ()
runMany fs = defaultMain $ run <$> fs