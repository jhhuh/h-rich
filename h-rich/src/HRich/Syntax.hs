{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HRich.Syntax
Description : Basic syntax highlighting.
Copyright   : (c) Ji-Haeng Huh, 2025
License     : BSD-3-Clause
Maintainer  : jhhuh.note@gmail.com

This module provides regex-based syntax highlighting for JSON and potentially other languages.
It converts raw source code into 'HRichText' with appropriate styles.
-}
module HRich.Syntax
    ( highlightJson
    ) where

import HRich.Text
import HRich.Style
import HRich.Color
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Functor (($>))

type Parser = Parsec Void Text

-- | Highlight a JSON string.
highlightJson :: Text -> HRichText
highlightJson input = case parse jsonParser "" input of
    Left _ -> fromPlain input
    Right spans -> HRichText input spans emptyStyle

jsonParser :: Parser [Span]
jsonParser = concat <$> many tokenP

tokenP :: Parser [Span]
tokenP = choice
    [ stringP
    , numberP
    , boolP
    , nullP
    , symbolP
    , whitespaceP
    ]

-- Styles
strStyle, numStyle, boolStyle, nullStyle, keyStyle :: Style
strStyle  = emptyStyle { color = Just (ANSI 2) } -- Green
keyStyle  = emptyStyle { color = Just (ANSI 4), bold = Just True } -- Blue Bold
numStyle  = emptyStyle { color = Just (ANSI 5) } -- Magenta
boolStyle = emptyStyle { color = Just (ANSI 3), italic = Just True } -- Yellow Italic
nullStyle = emptyStyle { color = Just (ANSI 1) } -- Red

whitespaceP :: Parser [Span]
whitespaceP = do
    _ <- some spaceChar
    return []

symbolP :: Parser [Span]
symbolP = do
    _ <- oneOf [',', ':', '{', '}', '[', ']']
    return []

stringP :: Parser [Span]
stringP = do
    start <- getOffset
    _ <- char '"'
    _ <- many (noneOf ['"'] <|> (char '\\' >> anySingle))
    _ <- char '"'
    end <- getOffset
    
    -- Check if it's a key (followed by colon)
    isKey <- optional (lookAhead (space >> char ':'))
    let style = case isKey of
            Just _ -> keyStyle
            Nothing -> strStyle
            
    return [Span start end style]

numberP :: Parser [Span]
numberP = do
    start <- getOffset
    _ <- L.scientific
    end <- getOffset
    return [Span start end numStyle]

boolP :: Parser [Span]
boolP = do
    start <- getOffset
    _ <- string "true" <|> string "false"
    end <- getOffset
    return [Span start end boolStyle]

nullP :: Parser [Span]
nullP = do
    start <- getOffset
    _ <- string "null"
    end <- getOffset
    return [Span start end nullStyle]
