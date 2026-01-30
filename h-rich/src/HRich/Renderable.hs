{-|
Module      : HRich.Renderable
Description : Typeclass for renderable terminal components.
Copyright   : (c) Ji-Haeng Huh, 2025
License     : BSD-3-Clause
Maintainer  : jhhuh.note@gmail.com

This module defines the 'Renderable' typeclass, which is the core interface
for anything that can be turned into styled segments for console output.
-}
module HRich.Renderable
    ( -- * Core Typeclass
      Renderable(..)
    , ConsoleOptions(..)
    ) where

import HRich.Segment
import HRich.Width (textWidth)
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
            maxWidth = maximum (0 : map (sum . map (textWidth . segmentText)) lines')
        in Measurement maxWidth maxWidth
