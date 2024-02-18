module Main (main) where

import Prelude hiding (writeFile, readFile)

import Options.Applicative
import Fmt ((+|), (|+))

import Data.Text.Lazy.IO (writeFile, readFile)
import Data.Text.Lazy (Text)

-- only for file search
import Test.Tasty.Golden (findByExtension)
import System.FilePath (replaceDirectory, replaceExtension, (</>), )

import qualified Reader (run)
import QTree (QTree, toText, explicitZeros)

data Input = MkInput { dstPath :: FilePath }

inputParser :: Parser Input
inputParser = MkInput
    <$> strOption
        (long "dstpath"
        <> short 'd'
        <> help "Destination path")

inputInfo :: ParserInfo Input
inputInfo = info inputParser
    ( fullDesc
    <> progDesc "Mtx to quad tree matrix converter" )

matrixName :: Text
matrixName = "matrix"

srcPaths :: IO [FilePath]
srcPaths = findByExtension [".mtx"] "dataset"

hvlLibPath :: FilePath
hvlLibPath = "hvl" </> "lib"

translateMtx :: QTree (Maybe Int) -> Text
translateMtx mtx =
    let expMtx = explicitZeros 0 mtx
    in toText expMtx

prependProgram :: Text -> Text -> Text -> Text
prependProgram varName matrix code = ""+|varName|+" = "+|matrix|+"\n"+|code|+""

mkDstPath :: FilePath -> FilePath -> FilePath
mkDstPath fp dst = (flip replaceExtension) "t" $ replaceDirectory fp dst

processMtx :: FilePath -> Text -> FilePath -> IO ()
processMtx dstPath code src = do
    mtx <- Reader.run src

    let mtxText = translateMtx mtx
    let code' = prependProgram matrixName mtxText code
    let dstPath' = mkDstPath src dstPath

    print dstPath'

    writeFile dstPath' code'

run :: FilePath -> IO ()
run dst = do
    hvlCode <- readFile hvlLibPath
    srcPaths' <- srcPaths

    mapM_ (processMtx dst hvlCode) srcPaths'

main :: IO ()
main = do
    MkInput { dstPath } <- execParser inputInfo

    run dstPath