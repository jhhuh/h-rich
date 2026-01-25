{-# LANGUAGE OverloadedStrings #-}
module HRich.Style
    ( Style(..)
    , emptyStyle
    , parseStyle
    , combineStyles
    ) where

import HRich.Color
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, catMaybes)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

data Style = Style
    { color     :: Maybe Color
    , bgColor   :: Maybe Color
    , bold      :: Maybe Bool
    , dim       :: Maybe Bool
    , italic    :: Maybe Bool
    , underline :: Maybe Bool
    , blink     :: Maybe Bool
    , inverse   :: Maybe Bool
    , strike    :: Maybe Bool
    } deriving (Show, Eq)

emptyStyle :: Style
emptyStyle = Style Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

combineStyles :: Style -> Style -> Style
combineStyles base overlay = Style
    { color     = color overlay     `firstJust` color base
    , bgColor   = bgColor overlay   `firstJust` bgColor base
    , bold      = bold overlay      `firstJust` bold base
    , dim       = dim overlay       `firstJust` dim base
    , italic    = italic overlay    `firstJust` italic base
    , underline = underline overlay `firstJust` underline base
    , blink     = blink overlay     `firstJust` blink base
    , inverse   = inverse   overlay     `firstJust` inverse base
    , strike    = strike    overlay     `firstJust` strike base
    }
  where
    firstJust (Just x) _ = Just x
    firstJust Nothing y  = y

type Parser = Parsec Void Text

parseStyle :: Text -> Style
parseStyle t = fromMaybe emptyStyle $ parseMaybe styleParser t

styleParser :: Parser Style
styleParser = do
    space
    parts <- many (lexeme partP)
    return $ foldl (flip ($)) emptyStyle parts
  where
    lexeme = L.lexeme space
    
    partP :: Parser (Style -> Style)
    partP = try onP <|> try notP <|> try aliasP <|> try colorP

    onP = do
        _ <- string "on"
        space
        cStr <- wordP
        case parseColor (T.pack cStr) of
            Just c -> return $ \s -> s { bgColor = Just c }
            Nothing -> fail "invalid background color"

    notP = do
        _ <- string "not"
        space
        alias <- wordP
        return $ \s -> disable alias s

    aliasP = do
        alias <- wordP
        case applyAlias alias of
            Just f -> return f
            Nothing -> fail "not an alias"

    colorP = do
        cStr <- wordP
        case parseColor (T.pack cStr) of
            Just c -> return $ \s -> s { color = Just c }
            Nothing -> fail "invalid color"

    wordP = (:) <$> (letterChar <|> char '#') <*> many (alphaNumChar <|> char '-')

    applyAlias "bold"      = Just $ \s -> s { bold = Just True }
    applyAlias "b"         = Just $ \s -> s { bold = Just True }
    applyAlias "dim"       = Just $ \s -> s { dim = Just True }
    applyAlias "italic"    = Just $ \s -> s { italic = Just True }
    applyAlias "i"         = Just $ \s -> s { italic = Just True }
    applyAlias "underline" = Just $ \s -> s { underline = Just True }
    applyAlias "u"         = Just $ \s -> s { underline = Just True }
    applyAlias "blink"     = Just $ \s -> s { blink = Just True }
    applyAlias "reverse"   = Just $ \s -> s { inverse = Just True }
    applyAlias "strike"    = Just $ \s -> s { strike = Just True }
    applyAlias "s"         = Just $ \s -> s { strike = Just True }
    applyAlias _           = Nothing

    disable "bold"      s = s { bold = Just False }
    disable "dim"       s = s { dim = Just False }
    disable "italic"    s = s { italic = Just False }
    disable "underline" s = s { underline = Just False }
    disable "blink"     s = s { blink = Just False }
    disable "reverse"   s = s { inverse = Just False }
    disable "strike"    s = s { strike = Just False }
    disable _           s = s

-- TODO: Refine Style parser to handle 'on' properly with Megaparsec combinators
