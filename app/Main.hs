module Main where

import Gen (gen)
import Parser (parseAndResolveModelFile)
import Paths_tlamodel (version)

import Data.Version (showVersion)
import Options.Applicative

data Args
  = Version
  | RunArgs
    { paths :: [String]
    , file :: String }

versionInfo :: String
versionInfo = "tlamodel " ++ showVersion version

versionP :: Parser Args
versionP = Version <$ flag' ()
    ( long "version"
    <> short 'v'
    <> help "show version")

runArgsP :: Parser Args
runArgsP = RunArgs
    <$> many (strOption
        ( long "search-path"
        <> short 's'
        <> metavar "PATH"
        <> help "model folder path"))
    <*> argument str (metavar "FILE")

opts :: ParserInfo Args
opts = info ((runArgsP <|> versionP) <**> helper)
  ( fullDesc
  <> progDesc (versionInfo ++ "\nConvert TLC model specification into CFG/TLA files")
  <> header "To simplify work with TLA specifications from command line")

main :: IO ()
main = do
  args <- execParser opts
  case args of
    Version -> putStrLn versionInfo
    options -> do
      model <- parseAndResolveModelFile ("." : paths options) $ file options
      gen model
