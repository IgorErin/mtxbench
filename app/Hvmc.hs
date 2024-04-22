module Hvmc (run) where

-- TODO duplication with Hvml

import qualified Hvml

import Path (mkDstPath)

import System.Process.Typed
import System.IO (openFile, Handle, IOMode (WriteMode))

import UnliftIO.Temporary (withSystemTempDirectory)

runHvmcCompile :: FilePath -> Handle -> IO ()
runHvmcCompile srcFile destHandle =
    runProcess_ $
    setStdout (useHandleClose destHandle) $
    proc "hvmc" [ "compile", srcFile ]

runFile :: FilePath -> FilePath -> IO FilePath
runFile dstFolder srcFile = do
    let destFile = mkDstPath srcFile "bench" dstFolder
    destHanle <- openFile destFile WriteMode

    runHvmcCompile srcFile destHanle

    return destFile

run :: FilePath -> FilePath -> IO [FilePath]
run srcFolder dstFolder = do
    withSystemTempDirectory "temp" $ \fp -> do
        src <- Hvml.run srcFolder fp

        mapM (runFile dstFolder) src
