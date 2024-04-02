module Main (main) where

import ArgParser as Args (Input(..), run)
import MtxTranslate as MT (run)
import Hvml (run)

main :: IO ()
main = do
    args <- Args.run

    case args of
        ConvertMatrix { dstPath } -> do
            _ <- MT.run dstPath

            return ()
        TranslateToHvmc { dstPath } -> do
            Hvml.run dstPath

            return ()
