{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | CLI for MinDSL parser
module Main where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec (errorBundlePretty)

import MinDSL.Parser (parseScale, parseScaleFile)
import MinDSL.JSON (encodeScale)

-- =============================================================================
-- CLI Options
-- =============================================================================

data Options = Options
  { optInput  :: InputSource
  , optOutput :: OutputDest
  , optFormat :: OutputFormat
  }

data InputSource
  = FileInput FilePath
  | StdinInput

data OutputDest
  = FileOutput FilePath
  | StdoutOutput

data OutputFormat
  = JSONFormat
  | PrettyJSON

-- =============================================================================
-- Option Parsers
-- =============================================================================

optionsParser :: Parser Options
optionsParser = Options
  <$> inputParser
  <*> outputParser
  <*> formatParser

inputParser :: Parser InputSource
inputParser =
  (FileInput <$> strOption
    ( long "input"
    <> short 'i'
    <> metavar "FILE"
    <> help "Input .mindsl file (use - for stdin)"
    ))
  <|> pure StdinInput

outputParser :: Parser OutputDest
outputParser =
  (FileOutput <$> strOption
    ( long "output"
    <> short 'o'
    <> metavar "FILE"
    <> help "Output JSON file (default: stdout)"
    ))
  <|> pure StdoutOutput

formatParser :: Parser OutputFormat
formatParser =
  flag PrettyJSON JSONFormat
    ( long "compact"
    <> short 'c'
    <> help "Compact JSON output (default: pretty-printed)"
    )

-- =============================================================================
-- Main
-- =============================================================================

main :: IO ()
main = do
  opts <- execParser optsInfo
  runParser opts
  where
    optsInfo = info (optionsParser <**> helper)
      ( fullDesc
      <> progDesc "Parse MinDSL files and output JSON AST"
      <> header "mindsl-parser - MinDSL parser"
      )

runParser :: Options -> IO ()
runParser Options{..} = do
  -- Read input
  input <- case optInput of
    FileInput "-" -> TIO.getContents
    FileInput path -> TIO.readFile path
    StdinInput -> TIO.getContents

  -- Parse
  let inputName = case optInput of
        FileInput path -> path
        StdinInput -> "<stdin>"

  case parseScale input of
    Left err -> do
      hPutStrLn stderr $ "Parse error in " ++ inputName ++ ":"
      hPutStrLn stderr $ errorBundlePretty err
      exitFailure

    Right scale -> do
      -- Output JSON
      let json = encodeScale scale
      case optOutput of
        FileOutput path -> BL.writeFile path json
        StdoutOutput -> BL.putStrLn json
