module Common (mkDstPath) where

import System.FilePath (replaceExtension, replaceDirectory)

mkDstPath :: FilePath -> FilePath ->  FilePath -> FilePath
mkDstPath fp ext dst =
    flip replaceExtension ext $
    replaceDirectory fp dst
