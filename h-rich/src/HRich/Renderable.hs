module HRich.Renderable
    ( Renderable(..)
    , ConsoleOptions(..)
    ) where

import HRich.Segment


data ConsoleOptions = ConsoleOptions
    { consoleWidth :: Int
    , consoleHeight :: Maybe Int
    } deriving (Show, Eq)

class Renderable a where
    render :: ConsoleOptions -> a -> [Segment]
    renderLines :: ConsoleOptions -> a -> [[Segment]]
    renderLines opts a = splitLines (render opts a)
