{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HRich.Progress
Description : Terminal progress bar component.
Copyright   : (c) Ji-Haeng Huh, 2025
License     : BSD-3-Clause
Maintainer  : jhhuh.note@gmail.com

This module defines the 'Progress' component, allowing the rendering of
completion bars with percentage indicators and optional labels.
-}
module HRich.Progress
    ( -- * Progress Type
      Progress(..)
      -- * Construction
    , progressBar
    ) where

import HRich.Renderable
import HRich.Segment
import HRich.Style
import HRich.Color
import Data.Text (Text)
import qualified Data.Text as T

data Progress = Progress
    { progressCurrent :: Double
    , progressTotal   :: Double
    , progressWidth   :: Maybe Int
    , progressLabel   :: Maybe Text
    } deriving (Show, Eq)

progressBar :: Double -> Double -> Progress
progressBar current total = Progress current total Nothing Nothing

instance Renderable Progress where
    render opts p = concat (renderLines opts p)
    
    renderLines opts p =
        let width = maybe (consoleWidth opts) id (progressWidth p)
            percentage = min 1.0 (max 0.0 (progressCurrent p / progressTotal p))
            
            label = maybe "" (`T.append` " ") (progressLabel p)
            labelLen = T.length label
            
            percentText = T.pack (show (round (percentage * 100) :: Int)) `T.append` "%"
            percentLen = T.length percentText
            
            -- "[...]" segments take 4 characters: [, ], space, space
            barWidth = max 0 (width - labelLen - percentLen - 4)
            filledWidth = round (fromIntegral barWidth * percentage)
            emptyWidth = barWidth - filledWidth
            
            filledStyle = emptyStyle { color = Just (ANSI 2) } -- Green
            emptyStyle' = emptyStyle { color = Just (ANSI 8) } -- Gray
            
            segments = [ Segment label Nothing
                       , Segment "[" Nothing
                       , Segment (T.replicate filledWidth "━") (Just filledStyle)
                       , Segment (T.replicate emptyWidth "━") (Just emptyStyle')
                       , Segment "] " Nothing
                       , Segment percentText Nothing
                       ]
        in [segments]
