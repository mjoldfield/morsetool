{-# LANGUAGE OverloadedStrings #-}

--
-- A very crude paragraph parser
--
-- Given some Text partition it into paragraphs
-- separated by at least one blank line.
--
-- Details:
--    - strip white space from start and end of lines;
--    - lines starting with # are treated as comments
--

module ParseInput
    ( parse
    ) where

import qualified Data.Text as T
import qualified Data.List.Split as LS

parse :: T.Text -> [T.Text]
parse = filter some
           . map smartJoin
           . LS.splitWhen T.null
           . filter (not . isComment)
           . map T.strip
           . T.lines

smartJoin :: [T.Text] -> T.Text
smartJoin = T.intercalate " " . filter some

isComment :: T.Text -> Bool
isComment = T.isPrefixOf "#"

some :: T.Text -> Bool
some = not . T.null


