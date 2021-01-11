module Main where

import Gen (gen)
import Parser (parseTplFile)

import Options.Applicative
import Data.Semigroup ((<>))

data Args = Args
  { paths     :: [String]
  , file      :: String }

args :: Parser Args
args = Args
    <$> many (strOption
        ( long "search-path"
        <> short 's'
        <> metavar "PATH"
        <> help "TPL folder path" ))
    <*> argument str (metavar "FILE")

opts :: ParserInfo Args
opts = info (args <**> helper)
  ( fullDesc
  <> progDesc "Convert template (TPL) into CFG/TLA files"
  <> header "To simplify work with TLA specifications from command line" )

main :: IO ()
main = do
    options <- execParser opts
    model <- parseTplFile ("." : paths options) $ file options
    gen model
