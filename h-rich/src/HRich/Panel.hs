{-# LANGUAGE OverloadedStrings #-}
module HRich.Panel
    ( Panel(..)
    , panel
    ) where

import HRich.Renderable
import HRich.Segment
import HRich.Box
import HRich.Style
import HRich.Text
import Data.Text (Text)
import qualified Data.Text as T

data Panel = Panel
    { panelRenderable :: HRichText -- For now only support Text
    , panelTitle      :: Maybe Text
    , panelBox        :: Box
    , panelStyle      :: Style
    , panelExpand     :: Bool
    }

panel :: HRichText -> Panel
panel r = Panel r Nothing rounded emptyStyle True

instance Renderable Panel where
    render options p =
        let width = consoleWidth options
            box' = panelBox p
            contentWidth = width - 2
            
            topLine = [Segment (top_left box' `T.append` T.replicate contentWidth (top box') `T.append` top_right box') (Just (panelStyle p))]
            newLine = [Segment "\n" Nothing]
            
            -- Render the content
            contentSegments = render options (panelRenderable p)
            
            -- Simplified: handle only single line for now
            -- Measure content length
            contentLen = sum [ T.length (segmentText s) | s <- contentSegments ]
            paddingLen = max 0 (contentWidth - contentLen)
            
            paddingSegment = Segment (T.replicate paddingLen " ") Nothing
            
            midLine = [ Segment (mid_left box') (Just (panelStyle p))
                      ] ++ contentSegments ++ [
                        paddingSegment,
                        Segment (mid_right box') (Just (panelStyle p))
                      ]
            
            bottomLine = [Segment (bottom_left box' `T.append` T.replicate contentWidth (bottom box') `T.append` bottom_right box') (Just (panelStyle p))]
            
        in topLine ++ newLine ++ midLine ++ newLine ++ bottomLine
