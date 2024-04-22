module ArgParser (Input(..), Action(..), run) where

import Options.Applicative
    ( Parser,
      ParserInfo,
      strOption,
      flag,
      long,
      short,
      help,
      info,
      fullDesc,
      progDesc,
      execParser )

data Action = Convert | Translate

data Input = Input { fl :: Action, srcPath:: FilePath, dstPath :: FilePath }

inputParser :: Parser Input
inputParser = Input
    <$> flag Convert Translate
        (long "trans"
        <> short 't'
        <> help "Enable verbose mode" )
    <*> strOption
        (long "srcPath"
        <> short 's'
        <> help "Destination path")
    <*> strOption
        (long "dstPath"
        <> short 'd'
        <> help "Destination path")

parser :: Parser Input
parser = inputParser

parserInfo :: ParserInfo Input
parserInfo = info parser
    ( fullDesc
    <> progDesc "Mtx to quad tree matrix converter" )

run :: IO Input
run = execParser parserInfo