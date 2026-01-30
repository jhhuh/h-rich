{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HRich.Text
Description : Rich text representation and parsing.
Copyright   : (c) Ji-Haeng Huh, 2025
License     : BSD-3-Clause
Maintainer  : jhhuh.note@gmail.com

This module defines 'HRichText', which represents styled text with spans.
It parses markup strings (e.g. "[bold]Text[/bold]") into a structured representation.
-}
module HRich.Text
    ( -- * Rich Text Types
      HRichText(..)
    , Span(..)
    , Justify(..)
      -- * Construction
    , fromPlain
    , fromMarkup
    , renderText
      -- * Justification
    , justified
    , leftJustify
    , centerJustify
    , rightJustify
    , fullJustify
    ) where

import HRich.Style
import HRich.Segment
import HRich.Renderable
import HRich.Width (textWidth)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sortOn)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void



data Span = Span
    { spanStart :: Int
    , spanEnd   :: Int
    , spanStyle :: Style
    } deriving (Show, Eq)

-- | Text justification mode
data Justify = JustifyLeft | JustifyCenter | JustifyRight | JustifyFull
    deriving (Show, Eq)

data HRichText = HRichText
    { plainText   :: Text
    , textSpans   :: [Span]
    , baseStyle   :: Style
    , textJustify :: Justify
    } deriving (Show, Eq)

instance Renderable HRichText where
    render _ = renderText
    renderLines opts t =
        let width = consoleWidth opts
            wrapped = wrapSegments width (renderText t)
        in map (justifyLine width (textJustify t)) wrapped

fromPlain :: Text -> HRichText
fromPlain t = HRichText t [] emptyStyle JustifyLeft

type Parser = Parsec Void Text

fromMarkup :: Text -> HRichText
fromMarkup input = case parse markupParser "" input of
    Left _ -> fromPlain input
    Right (plain, spans) -> HRichText plain spans emptyStyle JustifyLeft

markupParser :: Parser (Text, [Span])
markupParser = do
    chunks <- many (notFollowedBy (string "[/") >> chunkP)
    let (finalPlain, finalSpans) = foldl combine (T.empty, []) chunks
          where
            combine (tAcc, sAcc) (tChunk, sChunk) =
                let offset = T.length tAcc
                    sAdjusted = [ s { spanStart = spanStart s + offset, spanEnd = spanEnd s + offset } | s <- sChunk ]
                in (tAcc `T.append` tChunk, sAcc ++ sAdjusted)
    return (finalPlain, finalSpans)

chunkP :: Parser (Text, [Span])
chunkP = escapedP <|> taggedP <|> plainChunkP
  where
    escapedP = do
        _ <- char '\\'
        c <- anySingle
        return (T.singleton c, [])
        
    plainChunkP = do
        t <- some (noneOf ['[', '\\'])
        return (T.pack t, [])
        
    taggedP = do
        _ <- char '['
        styleName <- some (noneOf [']'])
        _ <- char ']'
        (innerPlain, innerSpans) <- markupParser
        _ <- string "[/"
        _ <- string (T.pack styleName)
        _ <- char ']'
        let style = parseStyle (T.pack styleName)
            outerSpan = Span 0 (T.length innerPlain) style
        return (innerPlain, outerSpan : innerSpans)

renderText :: HRichText -> [Segment]
renderText (HRichText plain spans base _) =
    let events = sortOn fst $
                 (0, Left base) :
                 [(T.length plain, Right ())] ++
                 concat [[(start, Left s), (end, Right ())] | Span start end s <- spans]

        getSegments [] _ _ _Segments = reverse _Segments
        getSegments ((pos, ev):rest) currentPos styleStack _Segments =
            let txt = T.take (pos - currentPos) (T.drop currentPos plain)
                currentStyle = foldl combineStyles emptyStyle styleStack
                newSegments = if T.null txt then _Segments else Segment txt (Just currentStyle) : _Segments
                newStack = case ev of
                    Left s -> styleStack ++ [s]
                    Right () -> if null styleStack then [] else init styleStack
            in getSegments rest pos newStack newSegments

    in getSegments events 0 [] []

-- | Set justification mode for text
justified :: Justify -> HRichText -> HRichText
justified j t = t { textJustify = j }

-- | Left justify text (default)
leftJustify :: HRichText -> HRichText
leftJustify = justified JustifyLeft

-- | Center justify text
centerJustify :: HRichText -> HRichText
centerJustify = justified JustifyCenter

-- | Right justify text
rightJustify :: HRichText -> HRichText
rightJustify = justified JustifyRight

-- | Full justify text (spread words to fill width)
fullJustify :: HRichText -> HRichText
fullJustify = justified JustifyFull

-- | Apply justification to a line of segments
justifyLine :: Int -> Justify -> [Segment] -> [Segment]
justifyLine width justify segments =
    let contentLen = sum [textWidth (segmentText s) | s <- segments]
        padding = max 0 (width - contentLen)
    in case justify of
        JustifyLeft -> segments ++ [Segment (T.replicate padding " ") Nothing]
        JustifyRight -> Segment (T.replicate padding " ") Nothing : segments
        JustifyCenter ->
            let leftPad = padding `div` 2
                rightPad = padding - leftPad
            in [Segment (T.replicate leftPad " ") Nothing] ++ segments ++ [Segment (T.replicate rightPad " ") Nothing]
        JustifyFull -> distributeSpaces width segments

-- | Distribute spaces evenly between words for full justification
distributeSpaces :: Int -> [Segment] -> [Segment]
distributeSpaces width segments =
    let contentLen = sum [textWidth (segmentText s) | s <- segments]
        totalPadding = max 0 (width - contentLen)
        -- Find space segments (word boundaries)
        isSpace seg = T.all (== ' ') (segmentText seg) && not (T.null (segmentText seg))
        spaceCount = length (filter isSpace segments)
    in if spaceCount == 0 || totalPadding == 0
       then segments ++ [Segment (T.replicate totalPadding " ") Nothing]
       else
           let extraPerSpace = totalPadding `div` spaceCount
               remainder = totalPadding `mod` spaceCount
               addExtra idx seg
                   | isSpace seg =
                       let extra = extraPerSpace + if idx < remainder then 1 else 0
                       in Segment (segmentText seg `T.append` T.replicate extra " ") (segmentStyle seg)
                   | otherwise = seg
           in zipWith addExtra [0..] segments
