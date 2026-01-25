{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
module HRich.Table
    ( Table(..)
    , TableColumn(..)
    , table, addColumn, addRow
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
    }

table :: Table
table = Table [] [] rounded emptyStyle Nothing True

addColumn :: T.Text -> Table -> Table
addColumn header (Table cols rows box' style' width' sh) =
    let newCol = TableColumn (fromMarkup header) Nothing Nothing "left"
    in Table (cols ++ [newCol]) rows box' style' width' sh

addRow :: [T.Text] -> Table -> Table
addRow row (Table cols rows box' style' width' sh) =
    let richRow = map fromMarkup row
    in Table cols (rows ++ [richRow]) box' style' width' sh

instance Renderable Table where
    render options t = concat (renderLines options t)

    renderLines options (Table cols rows' box' style' tWidth _) =
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

intercalateSegment :: Segment -> [[Segment]] -> [Segment]
intercalateSegment _ [] = []
intercalateSegment _ [x] = x
intercalateSegment sep (x:xs) = x ++ (sep : intercalateSegment sep xs)

padLinesTable :: Int -> Int -> [[Segment]] -> [[Segment]]
padLinesTable target n lines' =
    let currentLen = length lines'
        paddingNeeded = target - currentLen
        emptyLine = [Segment (T.replicate n " ") Nothing]
        paddedExisting = map (padLineToWidthTable n) lines'
    in paddedExisting ++ replicate paddingNeeded emptyLine

padLineToWidthTable :: Int -> [Segment] -> [Segment]
padLineToWidthTable n segments =
    let currentLen = sum [ T.length (segmentText s) | s <- segments ]
        needed = n - currentLen
    in if needed <= 0 
       then segments 
       else segments ++ [Segment (T.replicate needed " ") Nothing]
