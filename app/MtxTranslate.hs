module MtxTranslate (run, processMtx) where

import Path (mtxPaths)

import Fmt ((+|), (|+))

import qualified Data.Text.Lazy.IO as TIO (writeFile, readFile)
import Data.Text.Lazy (Text)

import System.FilePath (replaceDirectory, replaceExtension, (</>), )

import qualified Reader (run)
import QTree (QTree, toText, explicitZeros)

matrix1Name :: Text
matrix1Name = "matrix1"

matrix2Name :: Text
matrix2Name = "matrix2"

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

processMtx :: FilePath -> (FilePath, FilePath) -> IO FilePath
processMtx dstPath (src1, src2) = do
    mainCode <- hvlCode

    mtx1 <- Reader.run src1
    mtx2 <- Reader.run src2

    let mtx1Text = translateMtx mtx1
    let mtx2Text = translateMtx mtx2

    let code' = prependProgram matrix1Name mtx1Text
                $ prependProgram matrix2Name mtx2Text mainCode

    let dstPath' = mkDstPath src1 dstPath

    TIO.writeFile dstPath' code'

    return dstPath'

run :: FilePath -> IO [FilePath]
run dst = do
    srcPaths' <- Path.mtxPaths

    mapM (processMtx dst) srcPaths'
