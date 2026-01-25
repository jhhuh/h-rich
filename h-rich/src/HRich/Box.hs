{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HRich.Box
Description : Terminal box formatting characters.
Copyright   : (c) Ji-Haeng Huh, 2025
License     : BSD-3-Clause
Maintainer  : jhhuh.note@gmail.com

This module defines standard box-drawing characters used for Panels, Tables,
and other bordered components.
-}
module HRich.Box
    ( -- * Box Type
      Box(..)
      -- * Standard Styles
    , square
    , rounded
    , heavy
    , double
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
