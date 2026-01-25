{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : HRich.Columns
Description : Layout component for side-by-side rendering.
Copyright   : (c) Ji-Haeng Huh, 2025
License     : BSD-3-Clause
Maintainer  : jhhuh.note@gmail.com

This module defines the 'Columns' component, which arranges a list of
Renderable items horizontally with equal width distribution.
-}
module HRich.Columns
    ( -- * Columns Type
      Columns(..)
    , columns
    ) where

import HRich.Renderable
import HRich.Segment
import qualified Data.Text as T

data Columns = forall a. Renderable a => Columns
    { columnRenderables :: [a]
    , columnPadding     :: Int
    , columnExpand      :: Bool
    , columnEqual       :: Bool
    }

columns :: Renderable a => [a] -> Columns
columns rs = Columns rs 1 False False

instance Renderable Columns where
    render options c = concat (renderLines options c)

    renderLines options (Columns rs padding _ _) =
        let numCols = length rs
            width = consoleWidth options
            -- Crude distribution: total width minus padding divided by number of columns
            totalPadding = (numCols - 1) * padding
            colWidth = (width - totalPadding) `div` numCols
            
            colOptions = options { consoleWidth = colWidth }
            
            -- Render each item into lines
            allColLines = map (renderLines colOptions) rs
            
            -- Pad each column to the same number of lines
            maxLines = maximum (0 : map length allColLines)
            paddedColLines = map (padLines maxLines colWidth) allColLines
            
            joinLines lIdx = 
                intercalateSegment (Segment (T.replicate padding " ") Nothing) 
                                   (map (!! lIdx) paddedColLines)

        in map joinLines [0..maxLines-1]

padLines :: Int -> Int -> [[Segment]] -> [[Segment]]
padLines target n lines' =
    let currentLen = length lines'
        paddingNeeded = target - currentLen
        emptyLine = [Segment (T.replicate n " ") Nothing]
        -- Pad existing lines to width n
        paddedExisting = map (padToWidth n) lines'
    in paddedExisting ++ replicate paddingNeeded emptyLine
