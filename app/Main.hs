module Main (main) where

import ArgParser as Args (Result(..), Action(..), run)

import MtxTranslate as MT (run)
import Hvml (run)
import Hvmc (run)
import Bench (runMany)

main :: IO ()
main = do
    args <- Args.run

    let s = src args
    let d = dst args

    case action args of
        Convert -> do
            _ <- MT.run s d

            return ()
        Translate -> do
            _ <- Hvml.run s d

            return ()
        Compile -> do
            _ <- Hvmc.run s d

            return ()
        Bench -> do
            paths <- Hvmc.run s d

            runMany paths
        Unknown -> putStrLn "Unkown option"

