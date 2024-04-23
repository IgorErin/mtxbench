module Reader(run) where

import Data.Matrix.MatrixMarket (readMatrix)
import qualified Convert (run)
import QTree (QTree)

run :: FilePath -> IO (QTree (Maybe Int))
run fp = do
    mtx <- readMatrix fp

    return $ (const 1 <$>) <$> Convert.run mtx