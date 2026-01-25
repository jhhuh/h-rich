{-# LANGUAGE OverloadedStrings #-}
module HRich.Box
    ( Box(..)
    , square, rounded, heavy, double
    ) where

import Data.Text (Text)

data Box = Box
    { top_left      :: Text
    , top           :: Text
    , top_right     :: Text
    , mid_left      :: Text
    , mid           :: Text
    , mid_right     :: Text
    , bottom_left   :: Text
    , bottom        :: Text
    , bottom_right  :: Text
    } deriving (Show, Eq)

square :: Box
square = Box "┌" "─" "┐" "│" " " "│" "└" "─" "┘"

rounded :: Box
rounded = Box "╭" "─" "╮" "│" " " "│" "╰" "─" "╯"

heavy :: Box
heavy = Box "┏" "━" "┓" "┃" " " "┃" "┗" "━" "┛"

double :: Box
double = Box "╔" "═" "╗" "║" " " "║" "╚" "═" "╝"
