module Reader(run) where

import Data.Matrix.MatrixMarket (readMatrix)
import Convert (convert)
import QTree (QTree)

run :: FilePath -> IO (QTree (Maybe Int))
run fp = do
    mtx <- readMatrix fp
    return $ convert mtx
