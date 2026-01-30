{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HRich.Table
Description : Table component for tabular data.
Copyright   : (c) Ji-Haeng Huh, 2025
License     : BSD-3-Clause
Maintainer  : jhhuh.note@gmail.com

This module defines the 'Table' component, enabling the rendering of data
in rows and columns with configurable headers, borders, and alignment.
-}
module HRich.Table
    ( -- * Table Types
      Table(..)
    , TableColumn(..)
      -- * Construction
    , table
    , grid
    , addColumn
    , addRow
    , addRichRow
    ) where

import HRich.Renderable
import HRich.Segment
import HRich.Box
import HRich.Style
import HRich.Text
import qualified Data.Text as T

data TableColumn = TableColumn
    { columnHeader    :: HRichText
    , columnWidth     :: Maybe Int
    , columnRatio     :: Maybe Int
    , columnAlign     :: T.Text -- "left", "center", "right"
    }

data Table = Table
    { tableColumns :: [TableColumn]
    , tableRows    :: [[HRichText]]
    , tableBox     :: Box
    , tableStyle   :: Style
    , tableWidth   :: Maybe Int
    , showHeader   :: Bool
    , showBorder   :: Bool  -- ^ Whether to show borders
    , tablePadding :: Int   -- ^ Padding between columns (for grid)
    }

-- | Standard table with borders
table :: Table
table = Table [] [] rounded emptyStyle Nothing True True 0

-- | Grid table without borders (for layouts)
grid :: Table
grid = Table [] [] rounded emptyStyle Nothing False False 1

addColumn :: T.Text -> Table -> Table
addColumn header t =
    let newCol = TableColumn (fromMarkup header) Nothing Nothing "left"
    in t { tableColumns = tableColumns t ++ [newCol] }

addRow :: [T.Text] -> Table -> Table
addRow row t =
    let richRow = map fromMarkup row
    in t { tableRows = tableRows t ++ [richRow] }

-- | Add a row of pre-constructed HRichText values
addRichRow :: [HRichText] -> Table -> Table
addRichRow row t = t { tableRows = tableRows t ++ [row] }

instance Renderable Table where
    render options t = concat (renderLines options t)

    renderLines options tbl@(Table cols rows' box' style' tWidth _ border padding) =
        if border
        then renderBorderedTable options tbl
        else renderGridTable options tbl

-- | Render a table with borders
renderBorderedTable :: ConsoleOptions -> Table -> [[Segment]]
renderBorderedTable options (Table cols rows' box' style' tWidth _ _ _) =
    let width = maybe (consoleWidth options) id tWidth
        numCols = length cols
        totalDividers = numCols - 1
        availableWidth = width - 2 - totalDividers
        colWidth = availableWidth `div` max 1 numCols

        -- Render header
        headerLines = if null cols then [] else renderTableRow (map columnHeader cols) colWidth box' style' True

        -- Render rows
        rowLines = concatMap (\r -> renderTableRow r colWidth box' style' False) rows'

        -- Bottom border
        bottomLine = [Segment (boxBottomLeft box' `T.append` T.intercalate (boxBottomDivider box') (replicate numCols (T.replicate colWidth (boxBottom box'))) `T.append` boxBottomRight box') (Just style')]

    in headerLines ++ rowLines ++ [bottomLine]

-- | Render a grid (borderless table)
renderGridTable :: ConsoleOptions -> Table -> [[Segment]]
renderGridTable options (Table cols rows' _ _ tWidth _ _ padding) =
    let width = maybe (consoleWidth options) id tWidth
        numCols = max (length cols) (if null rows' then 0 else length (head rows'))
        paddingTotal = padding * (numCols - 1)
        availableWidth = width - paddingTotal
        colWidth = availableWidth `div` max 1 numCols
        colOptions = ConsoleOptions colWidth Nothing
        paddingSeg = Segment (T.replicate padding " ") Nothing

        renderGridRow :: [HRichText] -> [[Segment]]
        renderGridRow cells =
            let allCellLines = map (renderLines colOptions) cells
                maxLines = maximum (1 : map length allCellLines)
                paddedCellLines = map (padLinesTable maxLines colWidth) allCellLines
                joinLine lIdx =
                    let cellSegs = map (!! lIdx) paddedCellLines
                    in intercalateSegment paddingSeg cellSegs
            in map joinLine [0..maxLines-1]

    in concatMap renderGridRow rows'

renderTableRow :: Renderable a => [a] -> Int -> Box -> Style -> Bool -> [[Segment]]
renderTableRow items colWidth box' style' isHeader =
    let colOptions = ConsoleOptions colWidth Nothing
        allCellLines = map (renderLines colOptions) items
        maxLines = maximum (0 : map length allCellLines)
        paddedCellLines = map (padLinesTable maxLines colWidth) allCellLines
        
        -- Top border
        topBorderText = (if isHeader then boxTopLeft box' else boxMidLeft box') `T.append` 
                        T.intercalate (if isHeader then boxTopDivider box' else boxMidDivider box') (replicate (length items) (T.replicate colWidth (if isHeader then boxTop box' else boxMid box'))) `T.append` 
                        (if isHeader then boxTopRight box' else boxMidRight box')
        topBorder = [Segment topBorderText (Just style')]
        
        joinLines lIdx =
            let cells = map (!! lIdx) paddedCellLines
                sep = Segment (boxVerticalDivider box') (Just style')
                line = [Segment (boxVertical box') (Just style')] ++ 
                       intercalateSegment sep cells ++ 
                       [Segment (boxVertical box') (Just style')]
            in line
            
        contentLines = map joinLines [0..maxLines-1]
        
    in topBorder : contentLines


padLinesTable :: Int -> Int -> [[Segment]] -> [[Segment]]
padLinesTable target n lines' =
    let currentLen = length lines'
        paddingNeeded = target - currentLen
        emptyLine = [Segment (T.replicate n " ") Nothing]
        paddedExisting = map (padToWidth n) lines'
    in paddedExisting ++ replicate paddingNeeded emptyLine
