{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HRich.Theme
Description : Theme system for mapping logical names to styles.
Copyright   : (c) Ji-Haeng Huh, 2025
License     : BSD-3-Clause
Maintainer  : jhhuh.note@gmail.com

This module defines the 'Theme' type and provides a default theme.
Themes allow decoupling logical roles (e.g. "info", "error") from specific visual styles.
-}
module HRich.Theme
    ( Theme
    , defaultTheme
    , lookupStyle
    ) where

import HRich.Style
import HRich.Color
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- | A Theme is a mapping from style names to actual Styles.
type Theme = Map Text Style

-- | Look up a style in the theme, returning 'emptyStyle' if not found.
lookupStyle :: Theme -> Text -> Style
lookupStyle theme name = Map.findWithDefault emptyStyle name theme

-- | A default theme with standard colors for common log levels and UI elements.
defaultTheme :: Theme
defaultTheme = Map.fromList
    [ ("debug",    emptyStyle { color = Just (ANSI 8) })    -- Bright Black
    , ("info",     emptyStyle { color = Just (ANSI 4) })    -- Blue
    , ("warning",  emptyStyle { color = Just (ANSI 3) })    -- Yellow
    , ("error",    emptyStyle { color = Just (ANSI 1), bold = Just True }) -- Red Bold
    , ("critical", emptyStyle { color = Just (ANSI 1), bgColor = Just (ANSI 7), bold = Just True }) -- Red on White Bold
    , ("success",  emptyStyle { color = Just (ANSI 2) })    -- Green
    
    , ("repr.str", emptyStyle { color = Just (ANSI 2) })    -- Green strings
    , ("repr.num", emptyStyle { color = Just (ANSI 6) })    -- Cyan numbers
    , ("repr.bool",emptyStyle { color = Just (ANSI 5) })    -- Magenta booleans
    ]
