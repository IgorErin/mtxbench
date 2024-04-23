module ArgParser (Result(..), Action(..), run) where

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
      execParser)

data Action = Convert | Translate | Compile | Bench | Unknown

data Input = Input {
    convert :: Bool,
    translate :: Bool,
    compile :: Bool,
    bench :: Bool,
    srcPath :: FilePath,
    dstPath :: FilePath }

inputParser :: Parser Input
inputParser = Input
    <$> flag False True
        (long "convert"
        <> short 'c')
    <*> flag False True
        (long "translate"
        <> short 't')
    <*> flag False True
        (long "compile"
        <> short 'k' )
    <*> flag False True
        (long "bench"
        <> short 'b' )
    <*> strOption
        (long "srcPath"
        <> short 's'
        <> help "Destination path")
    <*> strOption
        (long "dstPath"
        <> short 'd'
        <> help "Destination path")

data Result = Result {
    action :: Action,
    src :: FilePath,
    dst :: FilePath }

actionOfInput :: Input -> Action
actionOfInput Input { convert = True } = Convert
actionOfInput Input { translate = True } = Translate
actionOfInput Input { compile = True } = Compile
actionOfInput Input { bench = True } = Bench
actionOfInput _ = Unknown

parser :: Parser Input
parser = inputParser

parserInfo :: ParserInfo Input
parserInfo = info parser
    ( fullDesc
    <> progDesc "Mtx to quad tree matrix converter" )

run :: IO Result
run = do
    input <- execParser parserInfo

    return $ Result {
        action = actionOfInput input,
        src = srcPath input,
        dst = dstPath input
    }
