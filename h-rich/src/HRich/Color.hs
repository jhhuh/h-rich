{-# LANGUAGE OverloadedStrings #-}
module HRich.Color 
    ( Color(..)
    , ColorSystem(..)
    , parseColor
    , toAnsiCodes
    ) where

import Data.Word (Word8)
import Data.Text (Text)
import qualified Data.Text as T

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Read (readMaybe)
import Numeric (readHex)

data Color
    = Default
    | ANSI Word8
    | Color256 Word8
    | RGB Word8 Word8 Word8
    deriving (Show, Eq, Ord)

data ColorSystem
    = NoColor
    | Standard   -- 8 colors
    | EightBit   -- 256 colors
    | TrueColor  -- 24-bit colors
    deriving (Show, Eq, Ord)

toAnsiCodes :: Color -> Bool -> [String]
toAnsiCodes color foreground =
    let prefix = if foreground then "38" else "48"
        basicPrefix, brightPrefix :: Int
        basicPrefix = if foreground then 30 else 40
        brightPrefix = if foreground then 90 else 100
    in case color of
        Default -> [if foreground then "39" else "49"]
        ANSI n
            | n < 8     -> [show (basicPrefix + fromIntegral n)]
            | n < 16    -> [show (brightPrefix + fromIntegral n - 8)]
            | otherwise -> [prefix, "5", show n]
        Color256 n -> [prefix, "5", show n]
        RGB r g b  -> [prefix, "2", show r, show g, show b]

parseColor :: Text -> Maybe Color
parseColor t
    | t == "default" = Just Default
    | T.null t = Nothing
    | T.head t == '#' = parseHex t
    | Just n <- parseParen "color" t = Just (Color256 n)
    | Just (r,g,b) <- parseRGB t = Just (RGB r g b)
    | otherwise = Map.lookup (T.toLower t) colorMap

parseParen :: Text -> Text -> Maybe Word8
parseParen prefix t = do
    rest <- T.stripPrefix prefix t
    inner <- T.stripPrefix "(" rest >>= T.stripSuffix ")"
    readMaybe (T.unpack (T.strip inner))

parseRGB :: Text -> Maybe (Word8, Word8, Word8)
parseRGB t = do
    inner <- T.stripPrefix "rgb(" t >>= T.stripSuffix ")"
    case T.splitOn "," inner of
        [r, g, b] -> do
            rv <- readMaybe (T.unpack (T.strip r))
            gv <- readMaybe (T.unpack (T.strip g))
            bv <- readMaybe (T.unpack (T.strip b))
            return (rv, gv, bv)
        _ -> Nothing

parseHex :: Text -> Maybe Color
parseHex t = do
    let hex = T.unpack $ T.drop 1 t
    case hex of
        [r1,r2,g1,g2,b1,b2] -> do
            [(rv, "")] <- Just $ readHex [r1,r2]
            [(gv, "")] <- Just $ readHex [g1,g2]
            [(bv, "")] <- Just $ readHex [b1,b2]
            return $ RGB rv gv bv
        _ -> Nothing

colorMap :: Map Text Color
colorMap = Map.fromList $
    [ ("black", ANSI 0), ("red", ANSI 1), ("green", ANSI 2), ("yellow", ANSI 3)
    , ("blue", ANSI 4), ("magenta", ANSI 5), ("cyan", ANSI 6), ("white", ANSI 7)
    , ("bright_black", ANSI 8), ("bright_red", ANSI 9), ("bright_green", ANSI 10), ("bright_yellow", ANSI 11)
    , ("bright_blue", ANSI 12), ("bright_magenta", ANSI 13), ("bright_cyan", ANSI 14), ("bright_white", ANSI 15)
    ] ++ htmlColors

htmlColors :: [(Text, Color)]
htmlColors =
    [ ("aliceblue", RGB 0xf0 0xf8 0xff)
    , ("antiquewhite", RGB 0xfa 0xeb 0xd7)
    , ("aqua", RGB 0x00 0xff 0xff)
    , ("aquamarine", RGB 0x7f 0xff 0xd4)
    , ("azure", RGB 0xf0 0xff 0xff)
    , ("beige", RGB 0xf5 0xf5 0xdc)
    , ("bisque", RGB 0xff 0xe4 0xc4)
    , ("blanchedalmond", RGB 0xff 0xeb 0xcd)
    , ("blueviolet", RGB 0x8a 0x2b 0xe2)
    , ("brown", RGB 0xa5 0x2a 0x2a)
    , ("burlywood", RGB 0xde 0xb8 0x87)
    , ("cadetblue", RGB 0x5f 0x9e 0xa0)
    , ("chartreuse", RGB 0x7f 0xff 0x00)
    , ("chocolate", RGB 0xd2 0x69 0x1e)
    , ("coral", RGB 0xff 0x7f 0x50)
    , ("cornflowerblue", RGB 0x64 0x95 0xed)
    , ("cornsilk", RGB 0xff 0xf8 0xdc)
    , ("crimson", RGB 0xdc 0x14 0x3c)
    , ("darkblue", RGB 0x00 0x00 0x8b)
    , ("darkcyan", RGB 0x00 0x8b 0x8b)
    , ("darkgoldenrod", RGB 0xb8 0x86 0x0b)
    , ("darkgray", RGB 0xa9 0xa9 0xa9)
    , ("darkgreen", RGB 0x00 0x64 0x00)
    , ("darkgrey", RGB 0xa9 0xa9 0xa9)
    , ("darkkhaki", RGB 0xb8 0xb7 0x6b)
    , ("darkmagenta", RGB 0x8b 0x00 0x8b)
    , ("darkolivegreen", RGB 0x55 0x6b 0x2f)
    , ("darkorange", RGB 0xff 0x8c 0x00)
    , ("darkorchid", RGB 0x99 0x32 0xcc)
    , ("darkred", RGB 0x8b 0x00 0x00)
    , ("darksalmon", RGB 0xe9 0x96 0x7a)
    , ("darkseagreen", RGB 0x8f 0xbc 0x8f)
    , ("darkslateblue", RGB 0x48 0x3d 0x8b)
    , ("darkslategray", RGB 0x2f 0x4f 0x4f)
    , ("darkslategrey", RGB 0x2f 0x4f 0x4f)
    , ("darkturquoise", RGB 0x00 0xce 0xd1)
    , ("darkviolet", RGB 0x94 0x00 0xd3)
    , ("deeppink", RGB 0xff 0x14 0x93)
    , ("deepskyblue", RGB 0x00 0xbf 0xff)
    , ("dimgray", RGB 0x69 0x69 0x69)
    , ("dimgrey", RGB 0x69 0x69 0x69)
    , ("dodgerblue", RGB 0x1e 0x90 0xff)
    , ("firebrick", RGB 0xb2 0x22 0x22)
    , ("floralwhite", RGB 0xff 0xfa 0xf0)
    , ("forestgreen", RGB 0x22 0x8b 0x22)
    , ("fuchsia", RGB 0xff 0x00 0xff)
    , ("gainsboro", RGB 0xdc 0xdc 0xdc)
    , ("ghostwhite", RGB 0xf8 0xf8 0xff)
    , ("gold", RGB 0xff 0xd7 0x00)
    , ("goldenrod", RGB 0xda 0xa5 0x20)
    , ("gray", RGB 0x80 0x80 0x80)
    , ("green", RGB 0x00 0x80 0x00)
    , ("greenyellow", RGB 0xad 0xff 0x2f)
    , ("grey", RGB 0x80 0x80 0x80)
    , ("honeydew", RGB 0xf0 0xff 0xf0)
    , ("hotpink", RGB 0xff 0x69 0xb4)
    , ("indianred", RGB 0xcd 0x5c 0x5c)
    , ("indigo", RGB 0x4b 0x00 0x82)
    , ("ivory", RGB 0xff 0xff 0xf0)
    , ("khaki", RGB 0xf0 0xe6 0x8c)
    , ("lavender", RGB 0xe6 0xe6 0xfa)
    , ("lavenderblush", RGB 0xff 0xf0 0xf5)
    , ("lawngreen", RGB 0x7c 0xfc 0x00)
    , ("lemonchiffon", RGB 0xff 0xfa 0xcd)
    , ("lightblue", RGB 0xad 0xd8 0xe6)
    , ("lightcoral", RGB 0xf0 0x80 0x80)
    , ("lightcyan", RGB 0xe0 0xff 0xff)
    , ("lightgoldenrodyellow", RGB 0xfa 0xfa 0xd2)
    , ("lightgray", RGB 0xd3 0xd3 0xd3)
    , ("lightgreen", RGB 0x90 0xee 0x90)
    , ("lightgrey", RGB 0xd3 0xd3 0xd3)
    , ("lightpink", RGB 0xff 0xb6 0xc1)
    , ("lightsalmon", RGB 0xff 0xa0 0x7a)
    , ("lightseagreen", RGB 0x20 0xb2 0xaa)
    , ("lightskyblue", RGB 0x87 0xce 0xfa)
    , ("lightslategray", RGB 0x77 0x88 0x99)
    , ("lightslategrey", RGB 0x77 0x88 0x99)
    , ("lightsteelblue", RGB 0xb0 0xc4 0xde)
    , ("lightyellow", RGB 0xff 0xff 0xe0)
    , ("lime", RGB 0x00 0xff 0x00)
    , ("limegreen", RGB 0x32 0xcd 0x32)
    , ("linen", RGB 0xfa 0xf0 0xe6)
    , ("magenta", RGB 0xff 0x00 0xff)
    , ("maroon", RGB 0x80 0x00 0x00)
    , ("mediumaquamarine", RGB 0x66 0xcd 0xaa)
    , ("mediumblue", RGB 0x00 0x00 0xcd)
    , ("mediumorchid", RGB 0xba 0x55 0xd3)
    , ("mediumpurple", RGB 0x93 0x70 0xdb)
    , ("mediumseagreen", RGB 0x3c 0xb3 0x71)
    , ("mediumslateblue", RGB 0x7b 0x68 0xee)
    , ("mediumspringgreen", RGB 0x00 0xfa 0x9a)
    , ("mediumturquoise", RGB 0x48 0xd1 0xcc)
    , ("mediumvioletred", RGB 0xc7 0x15 0x85)
    , ("midnightblue", RGB 0x19 0x19 0x70)
    , ("mintcream", RGB 0xf5 0xff 0xfa)
    , ("mistyrose", RGB 0xff 0xe4 0xe1)
    , ("moccasin", RGB 0xff 0xe4 0xb5)
    , ("navajowhite", RGB 0xff 0xde 0xad)
    , ("navy", RGB 0x00 0x00 0x80)
    , ("oldlace", RGB 0xfd 0xf5 0xe6)
    , ("olive", RGB 0x80 0x80 0x00)
    , ("olivedrab", RGB 0x6b 0x8e 0x23)
    , ("orange", RGB 0xff 0xa5 0x00)
    , ("orangered", RGB 0xff 0x45 0x00)
    , ("orchid", RGB 0xda 0x70 0xd6)
    , ("palegoldenrod", RGB 0xee 0xe8 0xaa)
    , ("palegreen", RGB 0x98 0xfb 0x98)
    , ("paleturquoise", RGB 0xaf 0xee 0xee)
    , ("palevioletred", RGB 0xdb 0x70 0x93)
    , ("papayawhip", RGB 0xff 0xef 0xd5)
    , ("peachpuff", RGB 0xff 0xda 0xb9)
    , ("peru", RGB 0xcd 0x85 0x3f)
    , ("pink", RGB 0xff 0xc0 0xcb)
    , ("plum", RGB 0xdd 0xa0 0xdd)
    , ("powderblue", RGB 0xb0 0xe0 0xe6)
    , ("purple", RGB 0x80 0x00 0x80)
    , ("rebeccapurple", RGB 0x66 0x33 0x99)
    , ("red", RGB 0xff 0x00 0x00)
    , ("rosybrown", RGB 0xbc 0x8f 0x8f)
    , ("royalblue", RGB 0x41 0x69 0xe1)
    , ("saddlebrown", RGB 0x8b 0x45 0x13)
    , ("salmon", RGB 0xfa 0x80 0x72)
    , ("sandybrown", RGB 0xf4 0xa4 0x60)
    , ("seagreen", RGB 0x2e 0x8b 0x57)
    , ("seashell", RGB 0xff 0xf5 0xee)
    , ("sienna", RGB 0xa0 0x52 0x2d)
    , ("silver", RGB 0xc0 0xc0 0xc0)
    , ("skyblue", RGB 0x87 0xce 0xeb)
    , ("slateblue", RGB 0x6a 0x5a 0xcd)
    , ("slategray", RGB 0x70 0x80 0x90)
    , ("slategrey", RGB 0x70 0x80 0x90)
    , ("snow", RGB 0xff 0xfa 0xfa)
    , ("springgreen", RGB 0x00 0xff 0x7f)
    , ("steelblue", RGB 0x46 0x82 0xb4)
    , ("tan", RGB 0xd2 0xb4 0x8c)
    , ("teal", RGB 0x00 0x80 0x80)
    , ("thistle", RGB 0xd8 0xbf 0xd8)
    , ("tomato", RGB 0xff 0x63 0x47)
    , ("turquoise", RGB 0x40 0xe0 0xd0)
    , ("violet", RGB 0xee 0x82 0xee)
    , ("wheat", RGB 0xf5 0xde 0xb3)
    , ("white", RGB 0xff 0xff 0xff)
    , ("whitesmoke", RGB 0xf5 0xf5 0xf5)
    , ("yellow", RGB 0xff 0xff 0x00)
    , ("yellowgreen", RGB 0x9a 0xcd 0x32)
    ]
