module Path (mkDstPath, mtxPaths) where

import System.FilePath (replaceExtension, replaceDirectory)
import Test.Tasty.Golden (findByExtension)

mkDstPath :: FilePath -> FilePath ->  FilePath -> FilePath
mkDstPath fp ext dst =
    flip replaceExtension ext $
    replaceDirectory fp dst

mtxPaths :: IO [FilePath]
mtxPaths = findByExtension [".mtx"] "dataset"
