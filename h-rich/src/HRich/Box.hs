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
    , invisible
    , horizontalLine
    , simple
    ) where

import Data.Text (Text)

-- | Box characters following Rich's convention
data Box = Box
    { boxTopLeft        :: Text  -- ^ Top-left corner
    , boxTop            :: Text  -- ^ Top edge
    , boxTopDivider     :: Text  -- ^ Top divider (between columns)
    , boxTopRight       :: Text  -- ^ Top-right corner
    , boxHeadRowLeft    :: Text  -- ^ Head row separator left (below header)
    , boxHeadRowHorizontal :: Text  -- ^ Head row separator horizontal
    , boxHeadRowCross   :: Text  -- ^ Head row separator cross
    , boxHeadRowRight   :: Text  -- ^ Head row separator right
    , boxRowLeft        :: Text  -- ^ Body row separator left (between rows)
    , boxRowHorizontal  :: Text  -- ^ Body row separator horizontal
    , boxRowCross       :: Text  -- ^ Body row separator cross
    , boxRowRight       :: Text  -- ^ Body row separator right
    , boxBottomLeft     :: Text  -- ^ Bottom-left corner
    , boxBottom         :: Text  -- ^ Bottom edge
    , boxBottomDivider  :: Text  -- ^ Bottom divider
    , boxBottomRight    :: Text  -- ^ Bottom-right corner
    , boxVertical       :: Text  -- ^ Vertical edge
    , boxVerticalDivider :: Text -- ^ Vertical divider (between columns)
    } deriving (Show, Eq)

square :: Box
square = Box
    "┌" "─" "┬" "┐"  -- top
    "├" "─" "┼" "┤"  -- head row (below header)
    "├" "─" "┼" "┤"  -- row (between body rows)
    "└" "─" "┴" "┘"  -- bottom
    "│" "│"          -- vertical

rounded :: Box
rounded = Box
    "╭" "─" "┬" "╮"  -- top
    "├" "─" "┼" "┤"  -- head row
    "├" "─" "┼" "┤"  -- row
    "╰" "─" "┴" "╯"  -- bottom
    "│" "│"          -- vertical

heavy :: Box
heavy = Box
    "┏" "━" "┳" "┓"  -- top
    "┣" "━" "╋" "┫"  -- head row
    "┣" "━" "╋" "┫"  -- row
    "┗" "━" "┻" "┛"  -- bottom
    "┃" "┃"          -- vertical

double :: Box
double = Box
    "╔" "═" "╦" "╗"  -- top
    "╠" "═" "╬" "╣"  -- head row
    "╠" "═" "╬" "╣"  -- row
    "╚" "═" "╩" "╝"  -- bottom
    "║" "║"          -- vertical

-- | Invisible box (no borders, just spaces)
invisible :: Box
invisible = Box
    " " " " " " " "  -- top
    " " " " " " " "  -- head row
    " " " " " " " "  -- row
    " " " " " " " "  -- bottom
    " " " "          -- vertical

-- | Horizontal line only (no vertical borders)
horizontalLine :: Box
horizontalLine = Box
    "─" "─" "─" "─"  -- top
    "─" "─" "─" "─"  -- head row
    "─" "─" "─" "─"  -- row
    "─" "─" "─" "─"  -- bottom
    " " " "          -- vertical

-- | Simple style - only header separator line (like Rich's SIMPLE)
simple :: Box
simple = Box
    " " " " " " " "  -- top (no border above header)
    " " "─" "─" " "  -- head row (line below header, continuous)
    " " " " " " " "  -- row (no separators between body rows)
    " " " " " " " "  -- bottom (no bottom border)
    " " " "          -- vertical (no vertical lines)
