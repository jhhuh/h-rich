{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HRich.Width
Description : Unicode-aware display width calculation.
Copyright   : (c) Ji-Haeng Huh, 2025
License     : BSD-3-Clause
Maintainer  : jhhuh.note@gmail.com

This module provides functions to calculate the display width of Unicode text,
properly handling CJK characters, emoji, and other wide/narrow characters.
-}
module HRich.Width
    ( -- * Width Calculation
      textWidth
    , charWidth
      -- * Width-aware Text Operations
    , takeWidth
    , dropWidth
    , padRightWidth
    , padLeftWidth
    , centerWidth
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (ord)

-- | Calculate the display width of a Text string.
-- CJK characters and emoji count as 2, most other characters as 1.
textWidth :: Text -> Int
textWidth = T.foldl' (\acc c -> acc + charWidth c) 0

-- | Get the display width of a single character.
-- Returns 0 for control/combining chars, 2 for wide chars, 1 otherwise.
charWidth :: Char -> Int
charWidth c
    | isControl c         = 0
    | isCombining c       = 0
    | isWide c            = 2
    | otherwise           = 1
  where
    cp = ord c

    -- Control characters (C0, C1, DEL)
    isControl ch = let o = ord ch in o < 32 || (o >= 0x7F && o < 0xA0)

    -- Combining characters (approximate ranges)
    isCombining _ =
        (cp >= 0x0300 && cp <= 0x036F) ||  -- Combining Diacritical Marks
        (cp >= 0x1AB0 && cp <= 0x1AFF) ||  -- Combining Diacritical Marks Extended
        (cp >= 0x1DC0 && cp <= 0x1DFF) ||  -- Combining Diacritical Marks Supplement
        (cp >= 0x20D0 && cp <= 0x20FF) ||  -- Combining Diacritical Marks for Symbols
        (cp >= 0xFE20 && cp <= 0xFE2F)     -- Combining Half Marks

    -- Wide characters (CJK, fullwidth forms, emoji, etc.)
    isWide _ =
        -- CJK Radicals Supplement to Yi Radicals
        (cp >= 0x2E80 && cp <= 0xA4CF) ||
        -- Hangul Jamo Extended-A
        (cp >= 0xA960 && cp <= 0xA97F) ||
        -- Hangul Syllables
        (cp >= 0xAC00 && cp <= 0xD7AF) ||
        -- Hangul Jamo Extended-B
        (cp >= 0xD7B0 && cp <= 0xD7FF) ||
        -- CJK Compatibility Ideographs
        (cp >= 0xF900 && cp <= 0xFAFF) ||
        -- Fullwidth ASCII variants
        (cp >= 0xFF01 && cp <= 0xFF60) ||
        -- Fullwidth brackets
        (cp >= 0xFFE0 && cp <= 0xFFE6) ||
        -- CJK Unified Ideographs Extension B onwards
        (cp >= 0x20000 && cp <= 0x2FFFD) ||
        (cp >= 0x30000 && cp <= 0x3FFFD) ||
        -- Common emoji ranges (approximate)
        (cp >= 0x1F300 && cp <= 0x1F9FF) ||  -- Misc Symbols and Pictographs, Emoticons, etc.
        (cp >= 0x2600 && cp <= 0x26FF) ||    -- Miscellaneous Symbols
        (cp >= 0x2700 && cp <= 0x27BF) ||    -- Dingbats
        (cp >= 0x231A && cp <= 0x231B) ||    -- Watch, Hourglass
        (cp >= 0x23E9 && cp <= 0x23F3) ||    -- Media control symbols
        (cp >= 0x23F8 && cp <= 0x23FA) ||    -- Media control symbols
        (cp >= 0x25AA && cp <= 0x25AB) ||    -- Squares
        (cp >= 0x25B6 && cp <= 0x25C0) ||    -- Triangles
        (cp >= 0x25FB && cp <= 0x25FE) ||    -- Squares
        (cp >= 0x2614 && cp <= 0x2615) ||    -- Umbrella, Hot Beverage
        (cp >= 0x2648 && cp <= 0x2653) ||    -- Zodiac
        (cp >= 0x267F && cp <= 0x267F) ||    -- Wheelchair
        (cp >= 0x2693 && cp <= 0x2693) ||    -- Anchor
        (cp >= 0x26A1 && cp <= 0x26A1) ||    -- High Voltage
        (cp >= 0x26AA && cp <= 0x26AB) ||    -- Circles
        (cp >= 0x26BD && cp <= 0x26BE) ||    -- Soccer, Baseball
        (cp >= 0x26C4 && cp <= 0x26C5) ||    -- Snowman, Sun
        (cp >= 0x26CE && cp <= 0x26CE) ||    -- Ophiuchus
        (cp >= 0x26D4 && cp <= 0x26D4) ||    -- No Entry
        (cp >= 0x26EA && cp <= 0x26EA) ||    -- Church
        (cp >= 0x26F2 && cp <= 0x26F3) ||    -- Fountain, Golf
        (cp >= 0x26F5 && cp <= 0x26F5) ||    -- Sailboat
        (cp >= 0x26FA && cp <= 0x26FA) ||    -- Tent
        (cp >= 0x26FD && cp <= 0x26FD) ||    -- Fuel Pump
        (cp >= 0x2702 && cp <= 0x2702) ||    -- Scissors
        (cp >= 0x2705 && cp <= 0x2705) ||    -- Check Mark
        (cp >= 0x2708 && cp <= 0x270D) ||    -- Airplane to Writing Hand
        (cp >= 0x270F && cp <= 0x270F) ||    -- Pencil
        (cp >= 0x2712 && cp <= 0x2712) ||    -- Black Nib
        (cp >= 0x2714 && cp <= 0x2714) ||    -- Check Mark
        (cp >= 0x2716 && cp <= 0x2716) ||    -- X Mark
        (cp >= 0x271D && cp <= 0x271D) ||    -- Latin Cross
        (cp >= 0x2721 && cp <= 0x2721) ||    -- Star of David
        (cp >= 0x2728 && cp <= 0x2728) ||    -- Sparkles
        (cp >= 0x2733 && cp <= 0x2734) ||    -- Eight Spoked Asterisk
        (cp >= 0x2744 && cp <= 0x2744) ||    -- Snowflake
        (cp >= 0x2747 && cp <= 0x2747) ||    -- Sparkle
        (cp >= 0x274C && cp <= 0x274C) ||    -- Cross Mark
        (cp >= 0x274E && cp <= 0x274E) ||    -- Cross Mark
        (cp >= 0x2753 && cp <= 0x2755) ||    -- Question Marks
        (cp >= 0x2757 && cp <= 0x2757) ||    -- Exclamation Mark
        (cp >= 0x2763 && cp <= 0x2764) ||    -- Heart Exclamation, Heart
        (cp >= 0x2795 && cp <= 0x2797) ||    -- Plus, Minus, Divide
        (cp >= 0x27A1 && cp <= 0x27A1) ||    -- Right Arrow
        (cp >= 0x27B0 && cp <= 0x27B0) ||    -- Curly Loop
        (cp >= 0x27BF && cp <= 0x27BF) ||    -- Double Curly Loop
        (cp >= 0x2934 && cp <= 0x2935) ||    -- Arrows
        (cp >= 0x2B05 && cp <= 0x2B07) ||    -- Arrows
        (cp >= 0x2B1B && cp <= 0x2B1C) ||    -- Squares
        (cp >= 0x2B50 && cp <= 0x2B50) ||    -- Star
        (cp >= 0x2B55 && cp <= 0x2B55) ||    -- Circle
        (cp >= 0x3030 && cp <= 0x3030) ||    -- Wavy Dash
        (cp >= 0x303D && cp <= 0x303D) ||    -- Part Alternation Mark
        (cp >= 0x3297 && cp <= 0x3297) ||    -- Circled Ideograph Congratulation
        (cp >= 0x3299 && cp <= 0x3299)       -- Circled Ideograph Secret

-- | Take characters from the left until we reach the specified display width.
takeWidth :: Int -> Text -> Text
takeWidth maxW txt = go 0 txt
  where
    go _ t | T.null t = T.empty
    go w t =
        let c = T.head t
            cw = charWidth c
            newW = w + cw
        in if newW > maxW
           then T.empty
           else T.cons c (go newW (T.tail t))

-- | Drop characters from the left until we've dropped the specified display width.
dropWidth :: Int -> Text -> Text
dropWidth targetW txt = go 0 txt
  where
    go _ t | T.null t = T.empty
    go w t =
        let c = T.head t
            cw = charWidth c
            newW = w + cw
        in if newW > targetW
           then t
           else go newW (T.tail t)

-- | Pad text on the right to reach the specified display width.
padRightWidth :: Int -> Text -> Text
padRightWidth targetW txt =
    let currentW = textWidth txt
        needed = targetW - currentW
    in if needed <= 0
       then txt
       else txt `T.append` T.replicate needed " "

-- | Pad text on the left to reach the specified display width.
padLeftWidth :: Int -> Text -> Text
padLeftWidth targetW txt =
    let currentW = textWidth txt
        needed = targetW - currentW
    in if needed <= 0
       then txt
       else T.replicate needed " " `T.append` txt

-- | Center text within the specified display width.
centerWidth :: Int -> Text -> Text
centerWidth targetW txt =
    let currentW = textWidth txt
        totalPad = targetW - currentW
        leftPad = totalPad `div` 2
        rightPad = totalPad - leftPad
    in if totalPad <= 0
       then txt
       else T.replicate leftPad " " `T.append` txt `T.append` T.replicate rightPad " "
