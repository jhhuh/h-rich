{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HRich.Theme
Description : Theme system for mapping logical names to styles.
Copyright   : (c) Ji-Haeng Huh, 2025
License     : BSD-3-Clause
Maintainer  : jhhuh.note@gmail.com

This module defines the 'Theme' type and provides pre-built themes.
Themes allow decoupling logical roles (e.g. "info", "error") from specific visual styles.
-}
module HRich.Theme
    ( -- * Theme Type
      Theme(..)
    , lookupStyle
      -- * Pre-built Themes
    , defaultTheme
    , draculaTheme
    , monokaiTheme
    , solarizedDarkTheme
    , solarizedLightTheme
    , nordTheme
      -- * Theme Utilities
    , mergeThemes
    , extendTheme
    ) where

import HRich.Style
import HRich.Color
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- | A Theme contains style mappings for different semantic elements.
data Theme = Theme
    { themeStyles     :: Map Text Style  -- ^ Style mappings by name
    , themeCodeBg     :: Maybe Color     -- ^ Background color for code blocks
    , themePanelBorder :: Style          -- ^ Style for panel borders
    , themeTableBorder :: Style          -- ^ Style for table borders
    } deriving (Show, Eq)

-- | Look up a style in the theme, returning 'emptyStyle' if not found.
lookupStyle :: Theme -> Text -> Style
lookupStyle theme name = Map.findWithDefault emptyStyle name (themeStyles theme)

-- | Merge two themes, with the second taking precedence.
mergeThemes :: Theme -> Theme -> Theme
mergeThemes t1 t2 = Theme
    { themeStyles = Map.union (themeStyles t2) (themeStyles t1)
    , themeCodeBg = maybe (themeCodeBg t1) Just (themeCodeBg t2)
    , themePanelBorder = if themePanelBorder t2 == emptyStyle then themePanelBorder t1 else themePanelBorder t2
    , themeTableBorder = if themeTableBorder t2 == emptyStyle then themeTableBorder t1 else themeTableBorder t2
    }

-- | Extend a theme with additional style mappings.
extendTheme :: [(Text, Style)] -> Theme -> Theme
extendTheme styles theme = theme { themeStyles = Map.union (Map.fromList styles) (themeStyles theme) }

-- | A default theme with standard ANSI colors.
defaultTheme :: Theme
defaultTheme = Theme
    { themeStyles = Map.fromList
        -- Log levels
        [ ("debug",    emptyStyle { color = Just (ANSI 8) })
        , ("info",     emptyStyle { color = Just (ANSI 4) })
        , ("warning",  emptyStyle { color = Just (ANSI 3) })
        , ("error",    emptyStyle { color = Just (ANSI 1), bold = Just True })
        , ("critical", emptyStyle { color = Just (ANSI 1), bgColor = Just (ANSI 7), bold = Just True })
        , ("success",  emptyStyle { color = Just (ANSI 2) })

        -- Repr types
        , ("repr.str",  emptyStyle { color = Just (ANSI 2) })
        , ("repr.num",  emptyStyle { color = Just (ANSI 6) })
        , ("repr.bool", emptyStyle { color = Just (ANSI 5) })

        -- Syntax highlighting
        , ("syntax.keyword",  emptyStyle { color = Just (ANSI 5), bold = Just True })
        , ("syntax.string",   emptyStyle { color = Just (ANSI 2) })
        , ("syntax.number",   emptyStyle { color = Just (ANSI 6) })
        , ("syntax.comment",  emptyStyle { color = Just (ANSI 8), italic = Just True })
        , ("syntax.function", emptyStyle { color = Just (ANSI 4) })
        , ("syntax.type",     emptyStyle { color = Just (ANSI 3) })
        , ("syntax.operator", emptyStyle { color = Just (ANSI 1) })
        , ("syntax.variable", emptyStyle { color = Just (ANSI 7) })

        -- UI elements
        , ("panel.title",  emptyStyle { bold = Just True })
        , ("panel.border", emptyStyle { color = Just (ANSI 4) })
        , ("table.header", emptyStyle { bold = Just True })
        , ("table.border", emptyStyle { color = Just (ANSI 8) })
        , ("progress.bar", emptyStyle { color = Just (ANSI 2) })
        , ("progress.bg",  emptyStyle { color = Just (ANSI 8) })
        , ("tree.guide",   emptyStyle { color = Just (ANSI 8) })

        -- Markdown
        , ("markdown.h1",     emptyStyle { color = Just (ANSI 5), bold = Just True })
        , ("markdown.h2",     emptyStyle { color = Just (ANSI 4), bold = Just True })
        , ("markdown.h3",     emptyStyle { color = Just (ANSI 6), bold = Just True })
        , ("markdown.code",   emptyStyle { color = Just (ANSI 2) })
        , ("markdown.link",   emptyStyle { color = Just (ANSI 4), underline = Just True })
        , ("markdown.quote",  emptyStyle { color = Just (ANSI 8), italic = Just True })
        ]
    , themeCodeBg = Nothing
    , themePanelBorder = emptyStyle { color = Just (ANSI 4) }
    , themeTableBorder = emptyStyle { color = Just (ANSI 8) }
    }

-- | Dracula theme - a dark theme with vibrant colors.
-- https://draculatheme.com/
draculaTheme :: Theme
draculaTheme = Theme
    { themeStyles = Map.fromList
        -- Dracula palette:
        -- Background: #282a36, Foreground: #f8f8f2
        -- Comment: #6272a4, Cyan: #8be9fd, Green: #50fa7b
        -- Orange: #ffb86c, Pink: #ff79c6, Purple: #bd93f9
        -- Red: #ff5555, Yellow: #f1fa8c

        -- Log levels
        [ ("debug",    emptyStyle { color = Just (RGB 98 114 164) })  -- Comment
        , ("info",     emptyStyle { color = Just (RGB 139 233 253) }) -- Cyan
        , ("warning",  emptyStyle { color = Just (RGB 255 184 108) }) -- Orange
        , ("error",    emptyStyle { color = Just (RGB 255 85 85), bold = Just True })   -- Red
        , ("critical", emptyStyle { color = Just (RGB 255 255 255), bgColor = Just (RGB 255 85 85), bold = Just True })
        , ("success",  emptyStyle { color = Just (RGB 80 250 123) })  -- Green

        -- Repr types
        , ("repr.str",  emptyStyle { color = Just (RGB 241 250 140) })  -- Yellow
        , ("repr.num",  emptyStyle { color = Just (RGB 189 147 249) })  -- Purple
        , ("repr.bool", emptyStyle { color = Just (RGB 255 121 198) })  -- Pink

        -- Syntax highlighting
        , ("syntax.keyword",  emptyStyle { color = Just (RGB 255 121 198), bold = Just True })  -- Pink
        , ("syntax.string",   emptyStyle { color = Just (RGB 241 250 140) })  -- Yellow
        , ("syntax.number",   emptyStyle { color = Just (RGB 189 147 249) })  -- Purple
        , ("syntax.comment",  emptyStyle { color = Just (RGB 98 114 164), italic = Just True })  -- Comment
        , ("syntax.function", emptyStyle { color = Just (RGB 80 250 123) })   -- Green
        , ("syntax.type",     emptyStyle { color = Just (RGB 139 233 253), italic = Just True })  -- Cyan
        , ("syntax.operator", emptyStyle { color = Just (RGB 255 121 198) })  -- Pink
        , ("syntax.variable", emptyStyle { color = Just (RGB 248 248 242) })  -- Foreground

        -- UI elements
        , ("panel.title",  emptyStyle { color = Just (RGB 255 121 198), bold = Just True })
        , ("panel.border", emptyStyle { color = Just (RGB 98 114 164) })
        , ("table.header", emptyStyle { color = Just (RGB 189 147 249), bold = Just True })
        , ("table.border", emptyStyle { color = Just (RGB 98 114 164) })
        , ("progress.bar", emptyStyle { color = Just (RGB 80 250 123) })
        , ("progress.bg",  emptyStyle { color = Just (RGB 68 71 90) })
        , ("tree.guide",   emptyStyle { color = Just (RGB 98 114 164) })

        -- Markdown
        , ("markdown.h1",     emptyStyle { color = Just (RGB 189 147 249), bold = Just True })
        , ("markdown.h2",     emptyStyle { color = Just (RGB 139 233 253), bold = Just True })
        , ("markdown.h3",     emptyStyle { color = Just (RGB 80 250 123), bold = Just True })
        , ("markdown.code",   emptyStyle { color = Just (RGB 241 250 140) })
        , ("markdown.link",   emptyStyle { color = Just (RGB 139 233 253), underline = Just True })
        , ("markdown.quote",  emptyStyle { color = Just (RGB 98 114 164), italic = Just True })
        ]
    , themeCodeBg = Just (RGB 40 42 54)  -- Dracula background
    , themePanelBorder = emptyStyle { color = Just (RGB 98 114 164) }
    , themeTableBorder = emptyStyle { color = Just (RGB 68 71 90) }
    }

-- | Monokai theme - classic dark theme popular with developers.
monokaiTheme :: Theme
monokaiTheme = Theme
    { themeStyles = Map.fromList
        -- Monokai palette:
        -- Background: #272822, Foreground: #f8f8f2
        -- Comment: #75715e, Yellow: #e6db74, Green: #a6e22e
        -- Orange: #fd971f, Red: #f92672, Purple: #ae81ff, Cyan: #66d9ef

        -- Log levels
        [ ("debug",    emptyStyle { color = Just (RGB 117 113 94) })   -- Comment
        , ("info",     emptyStyle { color = Just (RGB 102 217 239) })  -- Cyan
        , ("warning",  emptyStyle { color = Just (RGB 253 151 31) })   -- Orange
        , ("error",    emptyStyle { color = Just (RGB 249 38 114), bold = Just True })  -- Red
        , ("critical", emptyStyle { color = Just (RGB 255 255 255), bgColor = Just (RGB 249 38 114), bold = Just True })
        , ("success",  emptyStyle { color = Just (RGB 166 226 46) })   -- Green

        -- Syntax highlighting
        , ("syntax.keyword",  emptyStyle { color = Just (RGB 249 38 114), bold = Just True })   -- Red
        , ("syntax.string",   emptyStyle { color = Just (RGB 230 219 116) })  -- Yellow
        , ("syntax.number",   emptyStyle { color = Just (RGB 174 129 255) })  -- Purple
        , ("syntax.comment",  emptyStyle { color = Just (RGB 117 113 94), italic = Just True })  -- Comment
        , ("syntax.function", emptyStyle { color = Just (RGB 166 226 46) })   -- Green
        , ("syntax.type",     emptyStyle { color = Just (RGB 102 217 239), italic = Just True })  -- Cyan
        , ("syntax.operator", emptyStyle { color = Just (RGB 249 38 114) })   -- Red
        , ("syntax.variable", emptyStyle { color = Just (RGB 248 248 242) })  -- Foreground

        -- UI elements
        , ("panel.border", emptyStyle { color = Just (RGB 117 113 94) })
        , ("table.border", emptyStyle { color = Just (RGB 73 72 62) })
        ]
    , themeCodeBg = Just (RGB 39 40 34)  -- Monokai background
    , themePanelBorder = emptyStyle { color = Just (RGB 117 113 94) }
    , themeTableBorder = emptyStyle { color = Just (RGB 73 72 62) }
    }

-- | Solarized Dark theme - precision colors for machines and people.
-- https://ethanschoonover.com/solarized/
solarizedDarkTheme :: Theme
solarizedDarkTheme = Theme
    { themeStyles = Map.fromList
        -- Solarized palette:
        -- base03: #002b36, base02: #073642, base01: #586e75
        -- base00: #657b83, base0: #839496, base1: #93a1a1
        -- yellow: #b58900, orange: #cb4b16, red: #dc322f
        -- magenta: #d33682, violet: #6c71c4, blue: #268bd2
        -- cyan: #2aa198, green: #859900

        -- Log levels
        [ ("debug",    emptyStyle { color = Just (RGB 88 110 117) })   -- base01
        , ("info",     emptyStyle { color = Just (RGB 38 139 210) })   -- blue
        , ("warning",  emptyStyle { color = Just (RGB 181 137 0) })    -- yellow
        , ("error",    emptyStyle { color = Just (RGB 220 50 47), bold = Just True })  -- red
        , ("critical", emptyStyle { color = Just (RGB 255 255 255), bgColor = Just (RGB 220 50 47), bold = Just True })
        , ("success",  emptyStyle { color = Just (RGB 133 153 0) })    -- green

        -- Syntax highlighting
        , ("syntax.keyword",  emptyStyle { color = Just (RGB 133 153 0), bold = Just True })    -- green
        , ("syntax.string",   emptyStyle { color = Just (RGB 42 161 152) })   -- cyan
        , ("syntax.number",   emptyStyle { color = Just (RGB 42 161 152) })   -- cyan
        , ("syntax.comment",  emptyStyle { color = Just (RGB 88 110 117), italic = Just True })  -- base01
        , ("syntax.function", emptyStyle { color = Just (RGB 38 139 210) })   -- blue
        , ("syntax.type",     emptyStyle { color = Just (RGB 181 137 0) })    -- yellow
        , ("syntax.operator", emptyStyle { color = Just (RGB 133 153 0) })    -- green
        , ("syntax.variable", emptyStyle { color = Just (RGB 131 148 150) })  -- base0

        -- UI elements
        , ("panel.border", emptyStyle { color = Just (RGB 88 110 117) })
        , ("table.border", emptyStyle { color = Just (RGB 7 54 66) })
        ]
    , themeCodeBg = Just (RGB 0 43 54)  -- base03
    , themePanelBorder = emptyStyle { color = Just (RGB 88 110 117) }
    , themeTableBorder = emptyStyle { color = Just (RGB 7 54 66) }
    }

-- | Solarized Light theme - light variant of Solarized.
solarizedLightTheme :: Theme
solarizedLightTheme = Theme
    { themeStyles = Map.fromList
        -- Same colors, different base
        [ ("debug",    emptyStyle { color = Just (RGB 147 161 161) })  -- base1
        , ("info",     emptyStyle { color = Just (RGB 38 139 210) })   -- blue
        , ("warning",  emptyStyle { color = Just (RGB 181 137 0) })    -- yellow
        , ("error",    emptyStyle { color = Just (RGB 220 50 47), bold = Just True })  -- red
        , ("critical", emptyStyle { color = Just (RGB 255 255 255), bgColor = Just (RGB 220 50 47), bold = Just True })
        , ("success",  emptyStyle { color = Just (RGB 133 153 0) })    -- green

        -- Syntax highlighting
        , ("syntax.keyword",  emptyStyle { color = Just (RGB 133 153 0), bold = Just True })
        , ("syntax.string",   emptyStyle { color = Just (RGB 42 161 152) })
        , ("syntax.number",   emptyStyle { color = Just (RGB 42 161 152) })
        , ("syntax.comment",  emptyStyle { color = Just (RGB 147 161 161), italic = Just True })
        , ("syntax.function", emptyStyle { color = Just (RGB 38 139 210) })
        , ("syntax.type",     emptyStyle { color = Just (RGB 181 137 0) })
        , ("syntax.operator", emptyStyle { color = Just (RGB 133 153 0) })
        , ("syntax.variable", emptyStyle { color = Just (RGB 101 123 131) })  -- base00

        -- UI elements
        , ("panel.border", emptyStyle { color = Just (RGB 147 161 161) })
        , ("table.border", emptyStyle { color = Just (RGB 238 232 213) })  -- base2
        ]
    , themeCodeBg = Just (RGB 253 246 227)  -- base3 (light background)
    , themePanelBorder = emptyStyle { color = Just (RGB 147 161 161) }
    , themeTableBorder = emptyStyle { color = Just (RGB 238 232 213) }
    }

-- | Nord theme - arctic, north-bluish color palette.
-- https://www.nordtheme.com/
nordTheme :: Theme
nordTheme = Theme
    { themeStyles = Map.fromList
        -- Nord palette:
        -- Polar Night: #2e3440, #3b4252, #434c5e, #4c566a
        -- Snow Storm: #d8dee9, #e5e9f0, #eceff4
        -- Frost: #8fbcbb, #88c0d0, #81a1c1, #5e81ac
        -- Aurora: #bf616a, #d08770, #ebcb8b, #a3be8c, #b48ead

        -- Log levels
        [ ("debug",    emptyStyle { color = Just (RGB 76 86 106) })    -- nord3
        , ("info",     emptyStyle { color = Just (RGB 136 192 208) })  -- nord8 (frost)
        , ("warning",  emptyStyle { color = Just (RGB 235 203 139) })  -- nord13 (yellow aurora)
        , ("error",    emptyStyle { color = Just (RGB 191 97 106), bold = Just True })   -- nord11 (red aurora)
        , ("critical", emptyStyle { color = Just (RGB 255 255 255), bgColor = Just (RGB 191 97 106), bold = Just True })
        , ("success",  emptyStyle { color = Just (RGB 163 190 140) })  -- nord14 (green aurora)

        -- Syntax highlighting
        , ("syntax.keyword",  emptyStyle { color = Just (RGB 129 161 193), bold = Just True })  -- nord9
        , ("syntax.string",   emptyStyle { color = Just (RGB 163 190 140) })  -- nord14
        , ("syntax.number",   emptyStyle { color = Just (RGB 180 142 173) })  -- nord15
        , ("syntax.comment",  emptyStyle { color = Just (RGB 76 86 106), italic = Just True })  -- nord3
        , ("syntax.function", emptyStyle { color = Just (RGB 136 192 208) })  -- nord8
        , ("syntax.type",     emptyStyle { color = Just (RGB 143 188 187) })  -- nord7
        , ("syntax.operator", emptyStyle { color = Just (RGB 129 161 193) })  -- nord9
        , ("syntax.variable", emptyStyle { color = Just (RGB 216 222 233) })  -- nord4

        -- UI elements
        , ("panel.border", emptyStyle { color = Just (RGB 76 86 106) })
        , ("table.border", emptyStyle { color = Just (RGB 59 66 82) })
        ]
    , themeCodeBg = Just (RGB 46 52 64)  -- nord0
    , themePanelBorder = emptyStyle { color = Just (RGB 76 86 106) }
    , themeTableBorder = emptyStyle { color = Just (RGB 59 66 82) }
    }
