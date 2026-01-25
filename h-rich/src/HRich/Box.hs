{-# LANGUAGE OverloadedStrings #-}
module HRich.Box
    ( Box(..)
    , square, rounded, heavy, double
    ) where

import Data.Text (Text)

data Box = Box
    { boxTopLeft        :: Text
    , boxTop            :: Text
    , boxTopDivider     :: Text
    , boxTopRight       :: Text
    , boxMidLeft        :: Text
    , boxMid            :: Text
    , boxMidDivider     :: Text
    , boxMidRight       :: Text
    , boxBottomLeft     :: Text
    , boxBottom         :: Text
    , boxBottomDivider  :: Text
    , boxBottomRight    :: Text
    , boxVertical       :: Text
    , boxVerticalDivider :: Text
    } deriving (Show, Eq)

square :: Box
square = Box "┌" "─" "┬" "┐" "├" "─" "┼" "┤" "└" "─" "┴" "┘" "│" "│"

rounded :: Box
rounded = Box "╭" "─" "┬" "╮" "├" "─" "┼" "┤" "╰" "─" "┴" "╯" "│" "│"

heavy :: Box
heavy = Box "┏" "━" "┳" "┓" "┣" "━" "╋" "┫" "┗" "━" "┻" "┛" "┃" "┃"

double :: Box
double = Box "╔" "═" "╦" "╗" "╠" "═" "╬" "╣" "╚" "═" "╩" "╝" "║" "║"
