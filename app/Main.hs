module Main (main) where

import ArgParser as Args (Input(..), Action(..), run)
import MtxTranslate as MT (run)
import Hvml (run)

main :: IO ()
main = do
    args <- Args.run

    let src = srcPath args
    let dst = dstPath args

    case fl args of
        Convert -> do
            _ <- MT.run src dst

            return ()
        Translate -> do
            Hvml.run src dst

            return ()
