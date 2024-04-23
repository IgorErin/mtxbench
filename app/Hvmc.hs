module Hvmc (run) where

-- TODO duplication with Hvml

import qualified Hvml

import Path (mkDstPath)

import System.Process.Typed

import UnliftIO.Temporary (withSystemTempDirectory)

runHvmcCompile :: FilePath -> FilePath -> IO ()
runHvmcCompile srcFile destPath =
    runProcess_ $ proc "hvmc" [ "compile", "--output", destPath, srcFile ]

runFile :: FilePath -> FilePath -> IO FilePath
runFile dstFolder srcFile = do
    let destFile = mkDstPath srcFile "bench" dstFolder

    runHvmcCompile srcFile destFile

    return destFile

run :: FilePath -> FilePath -> IO [FilePath]
run srcFolder dstFolder = do
    withSystemTempDirectory "temp" $ \fp -> do
        src <- Hvml.run srcFolder fp

        mapM (runFile dstFolder) src
