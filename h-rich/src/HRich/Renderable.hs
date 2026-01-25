module HRich.Renderable
    ( Renderable(..)
    , ConsoleOptions(..)
    ) where

import HRich.Segment
import qualified Data.Text as T

data ConsoleOptions = ConsoleOptions
    { consoleWidth :: Int
    , consoleHeight :: Maybe Int
    } deriving (Show, Eq)

data Measurement = Measurement
    { minimumWidth :: Int
    , maximumWidth :: Int
    } deriving (Show, Eq)

class Renderable a where
    render :: ConsoleOptions -> a -> [Segment]
    renderLines :: ConsoleOptions -> a -> [[Segment]]
    renderLines opts a = splitLines (render opts a)
    
    measure :: ConsoleOptions -> a -> Measurement
    -- Default measurement: render and find max line length
    measure opts a =
        let lines' = renderLines opts a
            maxWidth = maximum (0 : map (sum . map (T.length . segmentText)) lines')
        in Measurement maxWidth maxWidth
