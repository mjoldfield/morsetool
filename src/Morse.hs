{-# LANGUAGE OverloadedStrings #-}

--
-- Given some Text convert it to Morse code
-- in a variety of encodings.
--
-- Besides the usual string of dots and dashes also return
-- a list of times corresponding to marks and spaces in
-- the tone sequence.
--


module Morse
    ( parse, Renderings(..)
    ) where

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Map.Strict  as M
import qualified Data.Char as C

type ErrMessage = T.Text

-- initially parse to a sequnce of these:
data MToken = Dit | Dah | ElementSpace | LetterSpace | WordSpace | Invalid
  deriving (Show, Eq, Ord, Enum)

isMark :: MToken -> Bool
isMark = (`elem` [ Dit, Dah ])

isSpace :: MToken -> Bool
isSpace = (`elem` [ ElementSpace, LetterSpace, WordSpace ])

-- return this:
data Renderings = Renderings { dotsDashes :: T.Text
                             , times      :: [Int]
                             , message    :: T.Text
                             , sumTimes   :: (Int,Int)
                             }
                deriving Show;

parse :: T.Text -> Either ErrMessage Renderings
parse t = annotate t . parseRaw $ t

parseRaw :: T.Text -> [MToken]
parseRaw  = L.intercalate [WordSpace]   . map parseWord . T.words

parseWord :: T.Text -> [MToken]
parseWord = L.intercalate [LetterSpace] . map parseChar . T.unpack

parseChar :: Char -> [MToken]
parseChar c = M.findWithDefault [Invalid] (C.toLower c) morseMap

--
-- A valid is token stream:
--   - alternates mark and space;
--   - starts with a mark;
--   - ends with a mark.
--
checkTokens :: [MToken] -> Bool
checkTokens = and . zipWith ($) (cycle [ isMark, isSpace ])

annotate :: T.Text -> [MToken] -> Either ErrMessage Renderings
annotate txt ts | checkTokens ts = Right $ renderAll txt ts
                | otherwise      = Left  $ "Unable to parse " `T.append` T.pack (show ts)

renderAll :: T.Text -> [MToken] -> Renderings
renderAll txt ts = Renderings { message    = txt
                              , dotsDashes = renderToDDs      ts
                              , times      = renderToTimes    ts
                              , sumTimes   = renderToSumTimes ts
                              }

render :: [a] -> [MToken] -> [a]
render cs = map ((cs !!) . fromEnum)

renderToDDs :: [MToken] -> T.Text
renderToDDs = T.pack . concat . render [ ".", "-", "", " ", " / ", "?" ]

renderToSumTimes :: [MToken] -> (Int,Int)
renderToSumTimes = collectTimes . renderToTimes

both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)

collectTimes :: [Int] -> (Int, Int)
collectTimes = both (sum . map snd) . L.partition fst . zip (cycle [True,False])

-- the apparent random numbers in the list are the lengths
-- of each MToken measured in dits.
renderToTimes :: [MToken] -> [Int]
renderToTimes = render [ 1,3, 1,3,7, 0 ]

morseMap :: M.Map Char [MToken]
morseMap = M.map tokenise textMap
  where tokenise = L.intercalate [ElementSpace] . map dd
        dd '.'   = [Dit]
        dd '-'   = [Dah]
        dd _     = [Invalid]

textMap :: M.Map Char String
textMap = M.fromList
          [('a', ".-")
          ,('b', "-...")
          ,('c', "-.-.")
          ,('d', "-..")
          ,('e', ".")
          ,('f', "..-.")
          ,('g', "--.")
          ,('h', "....")
          ,('i', "..")
          ,('j', ".---")
          ,('k', "-.-")
          ,('l', ".-..")
          ,('m', "--")
          ,('n', "-.")
          ,('o', "---")
          ,('p', ".--.")
          ,('q', "--.-")
          ,('r', ".-.")
          ,('s', "...")
          ,('t', "-")
          ,('u', "..-")
          ,('v', "...-")
          ,('w', ".--")
          ,('x', "-..-")
          ,('y', "-.--")
          ,('z', "--..")
          ,('=', "-...-")
          ,('?', "..--..")
          ,('/', "-..-.")
          ,(',', "--..--")
          ,('.', ".-.-.-")
          ,(':', "---...")
          ,('\'', ".----.")
          ,('-', "-....-")
          ,('(', "-.--.")
          ,(')', "-.--.-")
          ,('0', "-----")
          ,('1', ".----")
          ,('2', "..---")
          ,('3', "...--")
          ,('4', "....-")
          ,('5', ".....")
          ,('6', "-....")
          ,('7', "--...")
          ,('8', "---..")
          ,('9', "----.")
          ,('@', ".--.-.")]
