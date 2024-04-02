module Main (main) where

import Criterion.Main ( Benchmark, bench, nf, defaultMain, bgroup )
import QTree (QTree, explicitZeros)

import Path (mtxPaths)
import Reader (run)
import System.FilePath (takeFileName)

readTree :: FilePath -> IO (String, Tree)
readTree fp = do
  tree <- Reader.run fp

  let name = takeFileName fp
  let tree' = QTree.explicitZeros 0 tree

  return (name, tree')

type Tree = QTree Int
type MapF = Int -> Int

mapBenchOfTree :: MapF -> String -> Tree  -> Benchmark
mapBenchOfTree f name tree = bench name $ nf (f <$>) tree

mkMapBench :: MapF -> String -> IO Benchmark
mkMapBench f name = do
  paths <- mtxPaths
  bdata <- mapM readTree paths

  let benchs = uncurry (mapBenchOfTree f) <$> bdata

  return $ bgroup name benchs

main :: IO ()
main = do
  b <- mkMapBench succ "map"
  defaultMain [ b ]