{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HRich.Tree
Description : Hierarchical tree rendering.
Copyright   : (c) Ji-Haeng Huh, 2025
License     : BSD-3-Clause
Maintainer  : jhhuh.note@gmail.com

This module provides a 'Tree' component for visualizing hierarchical data
structures with connecting lines.
-}
module HRich.Tree
    ( Tree(..)
    , Node(..)
    ) where

import HRich.Renderable
import HRich.Segment
import HRich.Text
import Data.Text (Text)
import qualified Data.Text as T

-- | A tree node containing a value and a list of children.
data Node a = Node a [Node a]
    deriving (Show, Eq)

-- | A wrapper for rendering a 'Node' as a visual tree.
data Tree = Tree
    { treeRoot :: Node HRichText
    } deriving (Show, Eq)

instance Renderable Tree where
    render opts (Tree root) = concat (renderTreeLines opts root "")
    
    renderLines opts (Tree root) = renderTreeLines opts root ""

-- | Stub options for recurring children where width layout isn't propagated strictly yet.
stubOptions :: ConsoleOptions
stubOptions = ConsoleOptions 80 Nothing

renderTreeLines :: ConsoleOptions -> Node HRichText -> Text -> [[Segment]]
renderTreeLines opts (Node val children) prefix =
    renderNodeLines opts (Node val children) "" ""

nodeValue :: Node a -> a
nodeValue (Node v _) = v

renderNodeLines :: ConsoleOptions -> Node HRichText -> Text -> Text -> [[Segment]]
renderNodeLines opts (Node val children) currentPrefix childPrefix =
    let labelLines = renderLines opts val
        
        formatLabelLine idx line = 
            let p = if idx == 0 then currentPrefix else childPrefix
            in Segment p Nothing : line
            
        formattedLabel = zipWith formatLabelLine [0..] labelLines
        
        childrenLines = renderChildrenList childPrefix children
    in formattedLabel ++ childrenLines

renderChildrenList :: Text -> [Node HRichText] -> [[Segment]]
renderChildrenList _ [] = []
renderChildrenList prefix [lastChild] =
    renderNodeLines stubOptions lastChild (prefix `T.append` "└── ") (prefix `T.append` "    ")
renderChildrenList prefix (c:cs) =
    renderNodeLines stubOptions c (prefix `T.append` "├── ") (prefix `T.append` "│   ") 
    ++ renderChildrenList prefix cs
