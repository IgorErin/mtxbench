module MtxTranslate (run, processMtx) where

import Path (mtxPaths)

import Fmt ((+|), (|+))

import qualified Data.Text.Lazy.IO as TIO (writeFile, readFile)
import Data.Text.Lazy (Text)

import System.FilePath (replaceDirectory, replaceExtension, (</>), )

import qualified Reader (run)
import QTree (QTree, toText, explicitZeros)

matrixName :: Text
matrixName = "matrix"

hvlLibPath :: FilePath
hvlLibPath = "hvl" </> "lib"

translateMtx :: QTree (Maybe Int) -> Text
translateMtx mtx =
    let expMtx = explicitZeros 0 mtx
    in toText expMtx

prependProgram :: Text -> Text -> Text -> Text
prependProgram varName matrix code = ""+|varName|+" = "+|matrix|+"\n"+|code|+""

mkDstPath :: FilePath -> FilePath -> FilePath
mkDstPath fp dst = flip replaceExtension "t" $ replaceDirectory fp dst

hvlCode :: IO Text
hvlCode = TIO.readFile hvlLibPath

processMtx :: FilePath -> FilePath -> IO FilePath
processMtx dstPath src = do
    mtx <- Reader.run src
    mainCode <- hvlCode

    let mtxText = translateMtx mtx
    let code' = prependProgram matrixName mtxText mainCode
    let dstPath' = mkDstPath src dstPath

    TIO.writeFile dstPath' code'

    return dstPath'

run :: FilePath -> IO [FilePath]
run dst = do
    srcPaths' <- Path.mtxPaths

    mapM (processMtx dst) srcPaths'
