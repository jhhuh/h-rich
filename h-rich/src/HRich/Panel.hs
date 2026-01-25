{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
module HRich.Panel
    ( Panel(..)
    , panel
    ) where

import HRich.Renderable
import HRich.Segment
import HRich.Box
import HRich.Style
import Data.Text (Text)
import qualified Data.Text as T

data Panel = forall a. Renderable a => Panel
    { panelRenderable :: a
    , panelTitle      :: Maybe Text
    , panelBox        :: Box
    , panelStyle      :: Style
    , panelExpand     :: Bool
    }

panel :: Renderable a => a -> Panel
panel r = Panel r Nothing rounded emptyStyle True

instance Renderable Panel where
    render options p = concat (renderLines options p)

    renderLines options (Panel r _ box' style' _) =
        let width = consoleWidth options
            contentWidth = width - 2
            
            topLine = [Segment (boxTop box' `T.append` T.replicate contentWidth (boxTop box') `T.append` boxTopRight box') (Just style')]
            -- Wait, top_left was correct before.
            topLineFixed = [Segment (boxTopLeft box' `T.append` T.replicate contentWidth (boxTop box') `T.append` boxTopRight box') (Just style')]
            
            -- Render the content as lines
            contentLines = renderLines (options { consoleWidth = contentWidth }) r
            
            -- Function to render a single line with borders and padding
            renderPanelLine segments =
                let contentLen = sum [ T.length (segmentText s) | s <- segments ]
                    paddingLen = max 0 (contentWidth - contentLen)
                    paddingSegment = Segment (T.replicate paddingLen " ") Nothing
                in (Segment (boxVertical box') (Just style') : segments) ++ 
                   [paddingSegment, Segment (boxVertical box') (Just style')]

            midLines = map renderPanelLine contentLines
            
            bottomLine = [Segment (boxBottomLeft box' `T.append` T.replicate contentWidth (boxBottom box') `T.append` boxBottomRight box') (Just style')]
            
        in topLineFixed : midLines ++ [bottomLine]
