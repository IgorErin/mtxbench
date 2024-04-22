module Main (main) where

import ArgParser as Args (Input(..), Action(..), run)
import MtxTranslate as MT (run)
import Hvml (run)
import Hvmc (run)



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
            _ <- Hvml.run src dst

            return ()
        Bench -> do
            files <- Hvmc.run src dst

            -- run benchs

            return undefined
