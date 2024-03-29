module Main (main) where

import Options.Applicative
import Reader (run)
import QTree (toBuilder, explicitZeros)
import Data.Text.Lazy.IO (putStr)
import Fmt ((+|), (|+))
import Data.Text.Lazy (Text)

import Prelude hiding (putStr)

inputParser :: Parser FilePath
inputParser = strOption
    (long "path"
    <> short 'p'
    <> help "Path to mtx file")

inputInfo :: ParserInfo FilePath
inputInfo = info inputParser
    ( fullDesc
    <> progDesc "Mtx to quad tree matrix converter" )

varName :: Text
varName = "matrix"

main :: IO ()
main = do
    mtxFilePath <- execParser inputInfo

    -- hardcoded implicite zeros elimination. rerork
    qtMtx <- explicitZeros 0 <$> run mtxFilePath
    let qtText = toBuilder qtMtx

    -- hardcoded binding. rework
    let result = ""+|varName|+" = "+|qtText|+""

    putStr result