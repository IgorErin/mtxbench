module Hvml (run) where

import qualified MtxTranslate as MT (run)

import Path (mkDstPath)

import System.Process.Typed
import System.IO (openFile, Handle, IOMode (WriteMode))

import UnliftIO.Temporary (withSystemTempDirectory)

runHvmlCompile :: FilePath -> Handle -> IO ()
runHvmlCompile srcFile destHandle =
    runProcess_ $
    setStdout (useHandleClose destHandle) $
    proc "hvml" [ "compile", srcFile ]

runFile :: FilePath -> FilePath -> IO ()
runFile dstFolder srcFile = do
    let destFile = mkDstPath srcFile "hvmc" dstFolder
    destHanle <- openFile destFile WriteMode

    runHvmlCompile srcFile destHanle

run :: FilePath -> IO ()
run dstFolder = do
    withSystemTempDirectory "temp" $ (\fp -> do
        src <- MT.run fp

        mapM_ (runFile dstFolder) src )
