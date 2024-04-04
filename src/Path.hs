module Path (mkDstPath, mtxPaths) where

import System.FilePath (replaceExtension, replaceDirectory)
import Test.Tasty.Golden (findByExtension)

mkDstPath :: FilePath -> FilePath ->  FilePath -> FilePath
mkDstPath fp ext dst =
    flip replaceExtension ext $
    replaceDirectory fp dst

mtxPaths :: IO [(FilePath, FilePath)]
mtxPaths = do
    mtx <- findByExtension [".mtx"] "dataset"
    let mtx2 = (++ "2") <$> mtx

    return $ zip mtx mtx2
