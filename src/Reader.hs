module Reader(run) where

import Data.Matrix.MatrixMarket (readMatrix)
import qualified Convert (run)
import QTree (QTree, mergeQTree)

run :: FilePath -> IO (QTree (Maybe Int))
run fp = do
    putStrLn $ "reading: " ++ fp
    mtx <- readMatrix fp
    let converted = mergeQTree $ Convert.run mtx

    return $ (const 1 <$>) <$> converted