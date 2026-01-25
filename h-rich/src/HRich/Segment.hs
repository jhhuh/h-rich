{-# LANGUAGE OverloadedStrings #-}
module HRich.Segment
    ( Segment(..)
    , renderSegment
    , splitLines
    , wrapSegments
    ) where

import HRich.Style
import HRich.Color
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)

data Segment = Segment
    { segmentText  :: Text
    , segmentStyle :: Maybe Style
    } deriving (Show, Eq)

renderSegment :: Segment -> Text
renderSegment (Segment txt Nothing) = txt
renderSegment (Segment txt (Just style)) =
    let codes = styleToAnsi style
    in if null codes
       then txt
       else T.pack ("\x1b[" ++ intercalate ";" codes ++ "m") `T.append` txt `T.append` "\x1b[0m"

splitLines :: [Segment] -> [[Segment]]
splitLines [] = []
splitLines segments = go segments [] []
  where
    go [] [] linesAcc = reverse linesAcc
    go [] currentLine linesAcc = reverse (reverse currentLine : linesAcc)
    go (Segment txt style : rest) currentLine linesAcc =
        if T.null txt 
        then go rest currentLine linesAcc
        else case T.break (== '\n') txt of
            (before, restTxt) ->
                let segmentBefore = if T.null before then Nothing else Just (Segment before style)
                    newCurrentLine = maybe currentLine (: currentLine) segmentBefore
                in if T.null restTxt
                    then go rest newCurrentLine linesAcc
                    else -- restTxt starts with \n
                        let nextLineRest = T.drop 1 restTxt -- Remove \n
                        in go (Segment nextLineRest style : rest) [] (reverse newCurrentLine : linesAcc)

wrapSegments :: Int -> [Segment] -> [[Segment]]
wrapSegments width segments = 
    let allLines = splitLines segments
    in concatMap wrapLine allLines
  where
    wrapLine [] = [[]]
    wrapLine line = go line 0 [] []
    
    go [] _ currentLine linesAcc = reverse (reverse currentLine : linesAcc)
    go (Segment txt style : rest) currentWidth currentLine linesAcc =
        let words' = splitWords txt
        in processWords words' style rest currentWidth currentLine linesAcc

    processWords [] _ rest currentWidth currentLine linesAcc = go rest currentWidth currentLine linesAcc
    processWords (w:ws) style rest currentWidth currentLine linesAcc =
        let wLen = T.length w
        in if currentWidth + wLen <= width
           then processWords ws style rest (currentWidth + wLen) (Segment w style : currentLine) linesAcc
           else if wLen > width
                then -- Force split a single long word
                    let splitPos = width - currentWidth
                        (before, after) = T.splitAt splitPos w
                        newLinesAcc = reverse (Segment before style : currentLine) : linesAcc
                    in processWords (after:ws) style rest 0 [] newLinesAcc
                else -- Wrap to next line
                    let newLinesAcc = if null currentLine then linesAcc else reverse currentLine : linesAcc
                        trimmedW = if T.isPrefixOf " " w && T.length w > 1 then T.drop 1 w else w
                    in processWords ws style rest (T.length trimmedW) [Segment trimmedW style] newLinesAcc

    splitWords t = 
        let pieces = T.groupBy (\a b -> (a == ' ') == (b == ' ')) t
        in pieces

styleToAnsi :: Style -> [String]
styleToAnsi s = 
    concat 
    [ maybe [] (\c -> toAnsiCodes c True) (color s)
    , maybe [] (\c -> toAnsiCodes c False) (bgColor s)
    , if bold s == Just True then ["1"] else []
    , if dim s == Just True then ["2"] else []
    , if italic s == Just True then ["3"] else []
    , if underline s == Just True then ["4"] else []
    , if blink s == Just True then ["5"] else []
    , if inverse s == Just True then ["7"] else []
    , if strike s == Just True then ["9"] else []
    ]
