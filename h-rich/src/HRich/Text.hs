module HRich.Text
    ( HRichText(..)
    , Span(..)
    , fromPlain
    , fromMarkup
    , renderText
    ) where

import HRich.Style
import HRich.Segment
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sortOn)

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

fromPlain :: Text -> HRichText
fromPlain t = HRichText t [] emptyStyle

-- Basic BBCode-like parser based on Isocline/Rich logic
-- [style]content[/style]
fromMarkup :: Text -> HRichText
fromMarkup input = 
    let (plain, spans) = parseMarkup input
    in HRichText plain spans emptyStyle

parseMarkup :: Text -> (Text, [Span])
parseMarkup input = go (T.unpack input) 0 [] [] ""
  where
    -- go input currentPos openTags spans resultPlain
    go [] _ _ spans acc = (T.pack acc, spans)
    go ('[':'/':xs) pos ((startPos, styleName):openTags) spans acc =
        let (tagName, rest) = break (== ']') xs
            style = parseStyle (T.pack styleName)
            newSpan = Span startPos pos style
        in case rest of
            (']':next) -> go next pos openTags (newSpan : spans) acc
            _ -> go xs pos ((startPos, styleName):openTags) spans (acc ++ "[/") -- Malformed
    go ('[':xs) pos openTags spans acc =
        let (tagName, rest) = break (== ']') xs
        in case rest of
            (']':next) -> go next pos ((pos, tagName) : openTags) spans acc
            _ -> go xs (pos + 1) openTags spans (acc ++ "[")
    go (x:xs) pos openTags spans acc =
        go xs (pos + 1) openTags spans (acc ++ [x])

renderText :: HRichText -> [Segment]
renderText (HRichText plain spans base) =
    let -- Flatten spans into points of interest (start/end)
        events = sortOn fst $ 
                 (0, Left base) : 
                 [(len, Right ()) | let len = T.length plain] ++
                 concat [[(start, Left s), (end, Right ())] | Span start end s <- spans]
        
        -- This is a bit simplified. Rich uses a stack of styles.
        -- We'll use a simple stack too.
        
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

-- Note: The above renderText is a bit naive in how it handles overlapping spans.
-- In a real implementation, we should be more careful with the stack.
