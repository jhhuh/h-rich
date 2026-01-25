{-# LANGUAGE OverloadedStrings #-}
module HRich.Segment
    ( Segment(..)
    , renderSegment
    ) where

import HRich.Style
import HRich.Color
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)

data Segment = Segment
    { segmentText :: Text
    , segmentStyle :: Maybe Style
    } deriving (Show, Eq)

renderSegment :: Segment -> Text
renderSegment (Segment txt Nothing) = txt
renderSegment (Segment txt (Just style)) =
    let codes = styleToAnsi style
    in if null codes
       then txt
       else T.pack ("\x1b[" ++ intercalate ";" codes ++ "m") `T.append` txt `T.append` "\x1b[0m"

styleToAnsi :: Style -> [String]
styleToAnsi s = 
    concat 
        [ maybe [] (`toAnsiCodes` True) (color s)
        , maybe [] (`toAnsiCodes` False) (bgColor s)
        , if bold s == Just True      then ["1"] else []
        , if dim s == Just True       then ["2"] else []
        , if italic s == Just True    then ["3"] else []
        , if underline s == Just True then ["4"] else []
        , if blink s == Just True     then ["5"] else []
        , if inverse s == Just True   then ["7"] else []
        , if strike s == Just True    then ["9"] else []
        ]
