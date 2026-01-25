{-# LANGUAGE OverloadedStrings #-}
module HRich.Text
    ( HRichText(..)
    , Span(..)
    , fromPlain
    , fromMarkup
    , renderText
    ) where

import HRich.Style
import HRich.Segment
import HRich.Renderable
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

data HRichText = HRichText
    { plainText :: Text
    , textSpans :: [Span]
    , baseStyle :: Style
    } deriving (Show, Eq)

instance Renderable HRichText where
    render _ = renderText

fromPlain :: Text -> HRichText
fromPlain t = HRichText t [] emptyStyle

type Parser = Parsec Void Text

fromMarkup :: Text -> HRichText
fromMarkup input = case parse markupParser "" input of
    Left _ -> fromPlain input
    Right (plain, spans) -> HRichText plain spans emptyStyle

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
renderText (HRichText plain spans base) =
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
