module Hvml (run) where

import qualified MtxTranslate as MT (run)

import Path (mkDstPath)

import System.Process.Typed
    ( runProcess_, proc, setStdout, useHandleClose )
import System.IO (openFile, Handle, IOMode (WriteMode))

import UnliftIO.Temporary (withSystemTempDirectory)

runHvmlCompile :: FilePath -> Handle -> IO ()
runHvmlCompile srcFile destHandle =
    runProcess_ $
    setStdout (useHandleClose destHandle) $
    proc "hvml" [ "compile", srcFile ]

runFile :: FilePath -> FilePath -> IO FilePath
runFile dstFolder srcFile = do
    let destFile = mkDstPath srcFile "hvmc" dstFolder
    destHanle <- openFile destFile WriteMode

    runHvmlCompile srcFile destHanle

    return destFile

run :: FilePath -> FilePath -> IO [FilePath]
run srcFolder dstFolder = do
    withSystemTempDirectory "temp" $ \fp -> do
        src <- MT.run srcFolder fp

        mapM (runFile dstFolder) src
