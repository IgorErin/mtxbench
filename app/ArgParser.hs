module ArgParser (Input(..), run) where

import Options.Applicative

data Input =
    ConvertMatrix { dstPath :: FilePath }
    | TranslateToHvmc { dstPath :: FilePath }

convertParser :: Parser Input
convertParser = ConvertMatrix
    <$> strOption
        (long "lpath"
        <> short 'l'
        <> help "Destination path")

hvmcParser :: Parser Input
hvmcParser = TranslateToHvmc <$> strOption (long "cpath" <> short 'c')

parser :: Parser Input
parser = convertParser <|> hvmcParser

parserInfo :: ParserInfo Input
parserInfo = info parser
    ( fullDesc
    <> progDesc "Mtx to quad tree matrix converter" )

run :: IO Input
run = execParser parserInfo