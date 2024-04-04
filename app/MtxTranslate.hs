module MtxTranslate (run, processMtx) where

import qualified Path (mtxPaths, mains)

import Fmt ((+|), (|+))

import qualified Data.Text.Lazy.IO as TIO (writeFile, readFile)
import Data.Text.Lazy (Text)

import System.FilePath (takeBaseName, (</>), (<.>))

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

mkDstPath :: FilePath -> FilePath -> FilePath -> FilePath
mkDstPath mtxFp mainFp dst =
    let mtxName = takeBaseName mtxFp
        testFunName = takeBaseName mainFp

        resultName = ""+|mtxName|+"_"+|testFunName|+""
    in dst </> resultName <.> "hvml"

hvlCode :: IO Text
hvlCode = TIO.readFile hvlLibPath

processMtx :: FilePath -> FilePath -> (FilePath, FilePath) -> IO FilePath
processMtx mainPath dstPath (src1, src2) = do
    libCode <- hvlCode
    mainCode <- TIO.readFile mainPath

    mtx1 <- Reader.run src1
    mtx2 <- Reader.run src2

    let mtx1Text = translateMtx mtx1
    let mtx2Text = translateMtx mtx2

    let code' = prependProgram "main" mainCode
                $ prependProgram matrix1Name mtx1Text
                $ prependProgram matrix2Name mtx2Text libCode

    let dstPath' = mkDstPath src1 mainPath dstPath

    TIO.writeFile dstPath' code'

    return dstPath'

run :: FilePath -> IO [FilePath]
run dst = do
    mtxPaths <- Path.mtxPaths
    mains <- Path.mains

    print mains

    sequence $
        [ processMtx mainPath dst mtxPairPath
          | mainPath <- mains, mtxPairPath <- mtxPaths ]

