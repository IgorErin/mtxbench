module MtxTranslate (run, processMtx) where

import qualified Path (mtxPaths, mains)

import Fmt ((+|), (|+))

import qualified Data.Text.Lazy.IO as TIO (appendFile, readFile)
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
    let dstPath' = mkDstPath src1 mainPath dstPath

    libCode <- hvlCode
    TIO.appendFile dstPath' libCode

    mainCode <- TIO.readFile mainPath
    TIO.appendFile dstPath' mainCode

    let addAssign :: Text -> Text -> IO ()
        addAssign mtx name = do
            TIO.appendFile dstPath' $ ""+|name|+" = "
            TIO.appendFile dstPath'   mtx
            TIO.appendFile dstPath' "\n\n"

            return ()

    mtx1 <- Reader.run src1
    let mtx1Text = translateMtx mtx1
    addAssign mtx1Text matrix1Name

    mtx2 <- Reader.run src2
    let mtx2Text = translateMtx mtx2
    addAssign mtx2Text matrix2Name

    return dstPath'

newLine :: IO ()
newLine = putChar '\n'

run :: FilePath -> FilePath -> IO [FilePath]
run src dst = do
    mtxPaths <- Path.mtxPaths src

    putStrLn "Matrices:"
    mapM_ print mtxPaths
    newLine

    mains <- Path.mains

    putStrLn "Functions:"
    mapM_ print mains
    newLine

    sequence $
        [ processMtx mainPath dst mtxPairPath
          | mainPath <- mains, mtxPairPath <- mtxPaths ]
