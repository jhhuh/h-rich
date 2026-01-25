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
            
            topLine = [Segment (top_left box' `T.append` T.replicate contentWidth (top box') `T.append` top_right box') (Just style')]
            
            -- Render the content as lines
            contentLines = renderLines (options { consoleWidth = contentWidth }) r
            
            -- Function to render a single line with borders and padding
            renderPanelLine segments =
                let contentLen = sum [ T.length (segmentText s) | s <- segments ]
                    paddingLen = max 0 (contentWidth - contentLen)
                    paddingSegment = Segment (T.replicate paddingLen " ") Nothing
                in (Segment (mid_left box') (Just style') : segments) ++ 
                   [paddingSegment, Segment (mid_right box') (Just style')]

            midLines = map renderPanelLine contentLines
            
            bottomLine = [Segment (bottom_left box' `T.append` T.replicate contentWidth (bottom box') `T.append` bottom_right box') (Just style')]
            
        in topLine : midLines ++ [bottomLine]
