{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | CLI for MinDSL parser
module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec (errorBundlePretty)

import MinDSL.Parser (parseScale)
import MinDSL.JSON (encodeScale)
import MinDSL.TypeScript (generateTypeScriptFile)

-- =============================================================================
-- CLI Options
-- =============================================================================

data Command
  = ParseCommand Options
  | GenTypesCommand (Maybe FilePath)  -- Output file path

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

commandParser :: Parser Command
commandParser = subparser
  ( command "parse" (info (ParseCommand <$> optionsParser <**> helper)
      (progDesc "Parse a .mindsl file to JSON"))
  <> command "gen-types" (info (GenTypesCommand <$> genTypesOutputParser <**> helper)
      (progDesc "Generate TypeScript type definitions from AST"))
  )
  <|> (ParseCommand <$> optionsParser)  -- Default to parse command

genTypesOutputParser :: Parser (Maybe FilePath)
genTypesOutputParser = optional $ strOption
  ( long "output"
  <> short 'o'
  <> metavar "FILE"
  <> help "Output TypeScript file (default: stdout)"
  )

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
  cmd <- execParser optsInfo
  runCommand cmd
  where
    optsInfo = info (commandParser <**> helper)
      ( fullDesc
      <> progDesc "Parse MinDSL files and output JSON AST"
      <> header "mindsl-parser - MinDSL parser and type generator"
      )

runCommand :: Command -> IO ()
runCommand (ParseCommand opts) = runParser opts
runCommand (GenTypesCommand outputPath) = runGenTypes outputPath

runGenTypes :: Maybe FilePath -> IO ()
runGenTypes outputPath = do
  let tsContent = generateTypeScriptFile
  case outputPath of
    Just path -> TIO.writeFile path tsContent
    Nothing   -> TIO.putStrLn tsContent

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
