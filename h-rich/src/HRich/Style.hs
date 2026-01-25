{-# LANGUAGE OverloadedStrings #-}
module HRich.Style
    ( Style(..)
    , emptyStyle
    , parseStyle
    , combineStyles
    ) where

import HRich.Color
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

data Style = Style
    { color     :: Maybe Color
    , bgColor   :: Maybe Color
    , bold      :: Maybe Bool
    , dim       :: Maybe Bool
    , italic    :: Maybe Bool
    , underline :: Maybe Bool
    , blink     :: Maybe Bool
    , inverse   :: Maybe Bool
    , strike    :: Maybe Bool
    } deriving (Show, Eq)

emptyStyle :: Style
emptyStyle = Style Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

combineStyles :: Style -> Style -> Style
combineStyles base overlay = Style
    { color     = color overlay     `firstJust` color base
    , bgColor   = bgColor overlay   `firstJust` bgColor base
    , bold      = bold overlay      `firstJust` bold base
    , dim       = dim overlay       `firstJust` dim base
    , italic    = italic overlay    `firstJust` italic base
    , underline = underline overlay `firstJust` underline base
    , blink     = blink overlay     `firstJust` blink base
    , inverse   = inverse   overlay     `firstJust` inverse base
    , strike    = strike    overlay     `firstJust` strike base
    }
  where
    firstJust (Just x) _ = Just x
    firstJust Nothing y  = y

parseStyle :: Text -> Style
parseStyle t = foldl applyWord emptyStyle (T.words (T.toLower t))
  where
    applyWord s "bold"      = s { bold = Just True }
    applyWord s "not"       = s -- Handle "not bold" etc later if needed
    applyWord s "dim"       = s { dim = Just True }
    applyWord s "italic"    = s { italic = Just True }
    applyWord s "underline" = s { underline = Just True }
    applyWord s "blink"     = s { blink = Just True }
    applyWord s "reverse"   = s { inverse = Just True }
    applyWord s "strike"    = s { strike = Just True }
    applyWord s w
        | "on " `T.isPrefixOf` w = s { bgColor = parseColor (T.drop 3 w) }
        | otherwise = 
            case parseColor w of
                Just c -> s { color = Just c }
                Nothing -> s
