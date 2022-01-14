{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

--
-- Given some Morse Code, write it to C files
-- such that it can be compiled into programs which
-- know nothing about Morse.
--
-- Generate a header and source file.
--

module ToC
    ( render
    ) where

import qualified Data.Text as T

import Text.Printf

import NeatInterpolation
import Morse

render :: T.Text -> [Morse.Renderings] -> [ (String, T.Text) ]
render infile ms = [ ("h", headerFile infile)
                   , ("c", sourceFile infile ms)
                   ]

sourceFile :: T.Text ->  [Morse.Renderings] -> T.Text
sourceFile infile ms = T.intercalate paraBreak
                       [ preamble infile
                       , source nTimes timeArray nMsgs msgPtrs
                       ]
  where allTimes  = concatMap ((++ [0]) . times) ms
        timeArray = ppIntArray             allTimes
        nTimes    = ppInt . length       $ allTimes
        offsets   = 0:[i + 1 | (i,t) <- zip [0..] allTimes, t == 0]
        msgPtrs   = ppOffsetArray $ zip offsets ms
        nMsgs     = ppInt . length       $ ms

ppArray' :: T.Text -> [T.Text] -> T.Text
ppArray' sp ts = T.concat [ "{" <> sp, T.intercalate ("," <> sp) ts, sp <> "}" ]

ppArray :: [T.Text] -> T.Text
ppArray = ppArray' " "

ppInt :: Int -> T.Text
ppInt = T.pack . show

ppIntArray :: [Int] -> T.Text
ppIntArray = ppArray . map ppInt

ppOffsetArray :: [(Int, Morse.Renderings)] -> T.Text
ppOffsetArray = ppArray' ""
                . map (T.pack . uncurry ppOffset)

ppOffset :: Int -> Morse.Renderings -> String
ppOffset i r = printf " morse_time_data + %4d\n// %s\n// %s\n// %s\n\n"
               i (message r) (dotsDashes r) (ppSums . sumTimes $ r)

ppSums :: (Int,Int) -> String
ppSums (tm,ts) = printf "%d dits : %d (%0.1f%%) marks, %d (%0.1f%%) spaces"
                   (tm + ts) tm fm ts fs
  where fm = 100.0 * fromIntegral tm / fromIntegral (tm + ts) :: Double
        fs = 100.0 * fromIntegral ts / fromIntegral (tm + ts) :: Double


--
-- Key idea is to put all the timing data in one long
-- array, then have a separate array of pointers into
-- that array s.t. each message corresponds to a pointer.
--
-- Each message alternates mark,space,mark,space,...,mark
-- followed by zero.
--
source :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text
source nTimes allTimes nMsgs msgPtrs =
  [text|
      #include "morseout.h"

      static const uint8_t morse_time_data[];
      static const uint8_t * const morse_messages[];

      uint32_t morse_get_next_time(uint32_t n_msg, uint32_t * const msg_i)
      {
        if (n_msg >= n_morse_messages)
          return 0;

        uint8_t tau = morse_messages[n_msg][*msg_i];
        *msg_i = (tau == 0) ? 0 : *msg_i + 1;

        return tau;
      }

      // number of messages
      const uint32_t n_morse_messages = $nMsgs;

      // main array of times, $nTimes entries
      static const uint8_t morse_time_data[] = $allTimes;

      // pointers into the array above corresponding to messages
      static const uint8_t * const morse_messages[] =
        $msgPtrs;

  |]  <> "\n"

headerFile :: T.Text -> T.Text
headerFile infile = T.intercalate paraBreak
                    [ preamble infile
                    , header
                    ]

header :: T.Text
header =
    [text|
#ifndef _MORSEOUT_H_
#define _MORSEOUT_H_

// the number of messages we have defined
extern const uint32_t n_morse_messages;

// Return the next time (in dits) for message n_msg.
//
//   - silly values of n_msg return 0.
//
//   - changing n_msg between calls will break things, so
//     don't do it.
//
//   - *msg_i should be set to 0 before the first call, then
//     left alone. It isn't safe to try arbitrary values! This
//     call will increment the value, then reset it, so you
//     can just loop forever over it.
//
//   - *msg_i will be set to 0 at the end of the message.
//
//  BUG: it would be better to make all values of msg_n and msg_i safe.
//
extern uint32_t morse_get_next_time(uint32_t n_msg, uint32_t * const msg_i);

#endif
|] <> "\n"

paraBreak :: T.Text
paraBreak = "\n\n\n"

preamble :: T.Text -> T.Text
preamble infile =
    [text|
         /*
           This file was produced by the morse-tool
           from $infile

           DO NOT EDIT IT MANUALLY
         */

         #include <stdint.h>

    |]
