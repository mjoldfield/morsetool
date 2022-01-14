{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import qualified Morse
import qualified ParseInput
import qualified ToC

data MyConfig = MyConfig { inFile   :: FilePath
                         , outStem  :: FilePath
                         }
  deriving Show;

config :: Parser MyConfig
config = MyConfig
         <$> strOption
           (    long    "input"
             <> short   'i'
             <> metavar "FILENAME"
             <> value   "morse.cfg"
             <> showDefault
             <> help    "Input file of phrases")
         <*> strOption
           (    long    "output"
             <> short   'o'
             <> metavar "STEM"
             <> value   "morseout"
             <> showDefault
             <> help    "Stem for output files")

opts :: ParserInfo MyConfig
opts = info (config <**> helper)
         (fullDesc
           <> progDesc "A compiler for text into Morse code, formatted as a list of tone and silence"
           <> header   "Morse Tool")

main :: IO ()
main = execParser opts >>= runApp

runApp :: MyConfig -> IO ()
runApp c = T.readFile ifName
             >>= return . parse
             >>= either outError (outSuccess outName (T.pack ifName))
  where ifName  = inFile  c
        outName = outStem c

parse :: T.Text -> Either T.Text [Morse.Renderings]
parse = mapM Morse.parse . ParseInput.parse

outError :: T.Text -> IO ()
outError   e = T.putStrLn $ "Error:\n"   <> e

outSuccess :: FilePath -> T.Text -> [Morse.Renderings] -> IO ()
outSuccess stem infile = mapM_ writeStemFile . ToC.render infile
  where writeStemFile (suffix,text) = T.writeFile (stem ++ "." ++ suffix) text

