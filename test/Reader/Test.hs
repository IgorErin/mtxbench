module Reader.Test (tests) where

import Test.Tasty (testGroup, TestTree )
import Test.Tasty.Golden (goldenVsStringDiff, findByExtension)

import System.FilePath (replaceExtension, (</>))

import qualified Data.ByteString.Lazy as BL
import Data.Text.Lazy.Encoding as TLE (encodeUtf8)
import Text.Pretty.Simple (pShowNoColor)

import qualified Reader (run)
import QTree (QTree)

type TestFun = FilePath -> IO (QTree (Maybe Int))

tests :: IO TestTree
tests = testGroup "" <$> sequence
  [ mkTest "efim" Reader.run]

mkTest :: String -> TestFun -> IO TestTree
mkTest name testFun = do
  mtx' <- mtx
  testGroup "Reader" <$> mapM (mk name testFun) mtx'

mtx :: IO [FilePath]
mtx = findByExtension [".mtx"] $ "test" </> "Reader" </> "Golden"

mkResultPath :: FilePath -> FilePath
mkResultPath fp = replaceExtension fp "golden"

mk :: String -> TestFun -> FilePath -> IO TestTree
mk name testFun fp = do
  let qtree = TLE.encodeUtf8 . pShowNoColor <$> testFun fp
  let resultFile = mkResultPath fp ++ name
  let name' = name ++ fp

  return $ golden name' resultFile qtree

golden :: String -> FilePath -> IO BL.ByteString -> TestTree
golden name resultPath result =
  let diff :: FilePath -> FilePath -> [FilePath]
      diff ref new = ["diff", "-u", ref, new]

  in goldenVsStringDiff name diff resultPath result