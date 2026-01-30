{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HRich.Segment
Description : Styled text segments and line wrapping logic.
Copyright   : (c) Ji-Haeng Huh, 2025
License     : BSD-3-Clause
Maintainer  : jhhuh.note@gmail.com

This module defines 'Segment', the atomic unit of rendered styled text,
and provides utility functions for converting styles to ANSI codes,
wrapping text, and handling multi-line segments.
-}
module HRich.Segment
    ( -- * Segment Type
      Segment(..)
    , renderSegment
      -- * Layout Utilities
    , splitLines
    , wrapSegments
    , intercalateSegment
    , padToWidth
    , padToWidthWithStyle
    ) where

import HRich.Style
import HRich.Color
import HRich.Width (textWidth)
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

intercalateSegment :: Segment -> [[Segment]] -> [Segment]
intercalateSegment _ [] = []
intercalateSegment _ [x] = x
intercalateSegment sep (x:xs) = x ++ (sep : intercalateSegment sep xs)

padToWidth :: Int -> [Segment] -> [Segment]
padToWidth n segments =
    let currentLen = sum [ textWidth (segmentText s) | s <- segments ]
        needed = n - currentLen
    in if needed <= 0
       then segments
       else segments ++ [Segment (T.replicate needed " ") Nothing]

-- | Pad segments to width with a specific style for padding
padToWidthWithStyle :: Int -> Style -> [Segment] -> [Segment]
padToWidthWithStyle n style segments =
    let currentLen = sum [ textWidth (segmentText s) | s <- segments ]
        needed = n - currentLen
    in if needed <= 0
       then segments
       else segments ++ [Segment (T.replicate needed " ") (Just style)]

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
        let wLen = textWidth w
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
                        trimmedW = if T.isPrefixOf " " w && textWidth w > 1 then T.drop 1 w else w
                    in processWords ws style rest (textWidth trimmedW) [Segment trimmedW style] newLinesAcc

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
