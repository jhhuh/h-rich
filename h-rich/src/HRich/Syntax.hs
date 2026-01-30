{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HRich.Syntax
Description : Syntax highlighting for code blocks.
Copyright   : (c) Ji-Haeng Huh, 2025
License     : BSD-3-Clause
Maintainer  : jhhuh.note@gmail.com

This module provides syntax highlighting for various programming languages.
Includes support for line numbers, indent guides, and themed coloring.
-}
module HRich.Syntax
    ( -- * Syntax Type
      Syntax(..)
    , syntax
      -- * Legacy Functions
    , highlightJson
    , highlightHaskell
    , highlightPython
    ) where

import HRich.Text
import HRich.Style
import HRich.Color
import HRich.Segment
import HRich.Renderable
import HRich.Width (textWidth)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.List (intercalate)

type Parser = Parsec Void Text

-- | A syntax-highlighted code block.
data Syntax = Syntax
    { syntaxCode        :: Text         -- ^ The source code
    , syntaxLanguage    :: Text         -- ^ Language name (e.g., "python", "haskell")
    , syntaxLineNumbers :: Bool         -- ^ Show line numbers
    , syntaxIndentGuides :: Bool        -- ^ Show indent guides
    , syntaxStartLine   :: Int          -- ^ Starting line number
    , syntaxBgColor     :: Maybe Color  -- ^ Background color for the block
    } deriving (Show, Eq)

-- | Create a syntax-highlighted code block.
syntax :: Text -> Text -> Syntax
syntax code lang = Syntax
    { syntaxCode = code
    , syntaxLanguage = lang
    , syntaxLineNumbers = False
    , syntaxIndentGuides = False
    , syntaxStartLine = 1
    , syntaxBgColor = Nothing
    }

instance Renderable Syntax where
    render opts s = concat (renderLines opts s)

    renderLines opts s =
        let codeLines = T.lines (syntaxCode s)
            totalLines = length codeLines
            lineNumWidth = if syntaxLineNumbers s
                           then length (show (syntaxStartLine s + totalLines - 1)) + 1
                           else 0
            contentWidth = consoleWidth opts - lineNumWidth

            -- Styles
            lineNumStyle = emptyStyle { color = Just (RGB 100 100 100), dim = Just True }
            guideStyle = emptyStyle { color = Just (RGB 60 60 60) }

            -- Highlight based on language
            highlightLine lineText = case T.toLower (syntaxLanguage s) of
                "python"  -> highlightPythonLine lineText
                "haskell" -> highlightHaskellLine lineText
                "json"    -> highlightJsonLine lineText
                _         -> [Segment lineText Nothing]

            renderCodeLine lineNum lineText =
                let lineNumSeg = if syntaxLineNumbers s
                                 then [Segment (T.pack (padLeft lineNumWidth (show lineNum) ++ " ")) (Just lineNumStyle)]
                                 else []
                    indentGuides = if syntaxIndentGuides s
                                   then addIndentGuides guideStyle lineText
                                   else []
                    codeSegs = highlightLine lineText
                    allSegs = lineNumSeg ++ indentGuides ++ codeSegs
                in padToWidth (consoleWidth opts) allSegs

        in zipWith renderCodeLine [syntaxStartLine s ..] codeLines

-- | Add indent guide segments to the beginning of a line.
addIndentGuides :: Style -> Text -> [Segment]
addIndentGuides guideStyle lineText =
    let indent = T.takeWhile (== ' ') lineText
        indentLen = T.length indent
        numGuides = indentLen `div` 4  -- One guide every 4 spaces
        guides = T.concat [T.replicate 3 " " `T.append` "│" | _ <- [1..numGuides]]
    in if numGuides > 0
       then [Segment guides (Just guideStyle)]
       else []

-- | Left-pad a string to a given width.
padLeft :: Int -> String -> String
padLeft n s = replicate (n - length s) ' ' ++ s

-- Python highlighting
highlightPythonLine :: Text -> [Segment]
highlightPythonLine line = case parse pythonLineParser "" line of
    Left _ -> [Segment line Nothing]
    Right segs -> segs

pythonLineParser :: Parser [Segment]
pythonLineParser = concat <$> many pythonTokenP <* eof

pythonTokenP :: Parser [Segment]
pythonTokenP = choice
    [ pythonCommentP
    , pythonStringP
    , pythonKeywordP
    , pythonNumberP
    , pythonIdentP
    , pythonOpP
    , pythonWhitespaceP
    ]

pythonKeywords :: [Text]
pythonKeywords = ["def", "class", "if", "else", "elif", "for", "while", "try",
                  "except", "finally", "with", "as", "import", "from", "return",
                  "yield", "raise", "pass", "break", "continue", "and", "or",
                  "not", "in", "is", "lambda", "True", "False", "None", "async", "await"]

pythonCommentP :: Parser [Segment]
pythonCommentP = do
    start <- getOffset
    _ <- char '#'
    rest <- takeRest
    let comment = "#" `T.append` rest
    return [Segment comment (Just $ emptyStyle { color = Just (RGB 117 113 94), italic = Just True })]

pythonStringP :: Parser [Segment]
pythonStringP = do
    quote <- string "\"\"\"" <|> string "'''" <|> string "\"" <|> string "'"
    content <- case T.length quote of
        3 -> manyTill anySingle (string quote)
        _ -> many (noneOf [T.head quote] <|> try (char '\\' >> anySingle))
    let fullStr = quote `T.append` T.pack content `T.append` quote
    return [Segment fullStr (Just $ emptyStyle { color = Just (RGB 230 219 116) })]

pythonKeywordP :: Parser [Segment]
pythonKeywordP = do
    word <- some (alphaNumChar <|> char '_')
    let wordText = T.pack word
    if wordText `elem` pythonKeywords
       then return [Segment wordText (Just $ emptyStyle { color = Just (RGB 249 38 114), bold = Just True })]
       else return [Segment wordText Nothing]

pythonNumberP :: Parser [Segment]
pythonNumberP = do
    num <- some (digitChar <|> char '.')
    return [Segment (T.pack num) (Just $ emptyStyle { color = Just (RGB 174 129 255) })]

pythonIdentP :: Parser [Segment]
pythonIdentP = do
    word <- some (alphaNumChar <|> char '_')
    return [Segment (T.pack word) Nothing]

pythonOpP :: Parser [Segment]
pythonOpP = do
    op <- oneOf ("()[]{}:,=+-*/<>!@%^&|~." :: String)
    return [Segment (T.singleton op) (Just $ emptyStyle { color = Just (RGB 249 38 114) })]

pythonWhitespaceP :: Parser [Segment]
pythonWhitespaceP = do
    ws <- some spaceChar
    return [Segment (T.pack ws) Nothing]

-- Haskell highlighting
highlightHaskellLine :: Text -> [Segment]
highlightHaskellLine line = case parse haskellLineParser "" line of
    Left _ -> [Segment line Nothing]
    Right segs -> segs

haskellLineParser :: Parser [Segment]
haskellLineParser = concat <$> many haskellTokenP <* eof

haskellTokenP :: Parser [Segment]
haskellTokenP = choice
    [ haskellCommentP
    , haskellStringP
    , haskellKeywordP
    , haskellTypeP
    , haskellNumberP
    , haskellIdentP
    , haskellOpP
    , haskellWhitespaceP
    ]

haskellKeywords :: [Text]
haskellKeywords = ["module", "where", "import", "qualified", "as", "hiding",
                   "data", "newtype", "type", "class", "instance", "deriving",
                   "if", "then", "else", "case", "of", "let", "in", "do",
                   "return", "pure"]

haskellCommentP :: Parser [Segment]
haskellCommentP = do
    _ <- string "--"
    rest <- takeRest
    let comment = "--" `T.append` rest
    return [Segment comment (Just $ emptyStyle { color = Just (RGB 117 113 94), italic = Just True })]

haskellStringP :: Parser [Segment]
haskellStringP = do
    _ <- char '"'
    content <- many (noneOf ['"'] <|> try (char '\\' >> anySingle))
    _ <- char '"'
    let fullStr = "\"" `T.append` T.pack content `T.append` "\""
    return [Segment fullStr (Just $ emptyStyle { color = Just (RGB 230 219 116) })]

haskellKeywordP :: Parser [Segment]
haskellKeywordP = try $ do
    word <- some (alphaNumChar <|> char '_' <|> char '\'')
    let wordText = T.pack word
    if wordText `elem` haskellKeywords
       then return [Segment wordText (Just $ emptyStyle { color = Just (RGB 249 38 114), bold = Just True })]
       else fail "not a keyword"

haskellTypeP :: Parser [Segment]
haskellTypeP = try $ do
    first <- upperChar
    rest <- many (alphaNumChar <|> char '_' <|> char '\'')
    let typeText = T.pack (first : rest)
    return [Segment typeText (Just $ emptyStyle { color = Just (RGB 102 217 239), italic = Just True })]

haskellNumberP :: Parser [Segment]
haskellNumberP = do
    num <- some (digitChar <|> char '.')
    return [Segment (T.pack num) (Just $ emptyStyle { color = Just (RGB 174 129 255) })]

haskellIdentP :: Parser [Segment]
haskellIdentP = do
    word <- some (alphaNumChar <|> char '_' <|> char '\'')
    return [Segment (T.pack word) Nothing]

haskellOpP :: Parser [Segment]
haskellOpP = do
    op <- oneOf ("()[]{}:,=+-*/<>!@#$%^&|~.;`\\" :: String)
    return [Segment (T.singleton op) (Just $ emptyStyle { color = Just (RGB 249 38 114) })]

haskellWhitespaceP :: Parser [Segment]
haskellWhitespaceP = do
    ws <- some spaceChar
    return [Segment (T.pack ws) Nothing]

-- JSON highlighting (legacy)
highlightJsonLine :: Text -> [Segment]
highlightJsonLine line = case parse jsonLineParser "" line of
    Left _ -> [Segment line Nothing]
    Right segs -> segs

jsonLineParser :: Parser [Segment]
jsonLineParser = concat <$> many jsonTokenP <* eof

jsonTokenP :: Parser [Segment]
jsonTokenP = choice
    [ jsonStringP
    , jsonNumberP
    , jsonBoolP
    , jsonNullP
    , jsonSymbolP
    , jsonWhitespaceP
    ]

jsonStringP :: Parser [Segment]
jsonStringP = do
    _ <- char '"'
    content <- many (noneOf ['"'] <|> try (char '\\' >> anySingle))
    _ <- char '"'
    let fullStr = "\"" `T.append` T.pack content `T.append` "\""
    -- Simple heuristic: strings followed by : are keys
    return [Segment fullStr (Just $ emptyStyle { color = Just (ANSI 2) })]

jsonNumberP :: Parser [Segment]
jsonNumberP = do
    num <- some (digitChar <|> oneOf ['.', '-', '+', 'e', 'E'])
    return [Segment (T.pack num) (Just $ emptyStyle { color = Just (ANSI 5) })]

jsonBoolP :: Parser [Segment]
jsonBoolP = do
    b <- string "true" <|> string "false"
    return [Segment b (Just $ emptyStyle { color = Just (ANSI 3), italic = Just True })]

jsonNullP :: Parser [Segment]
jsonNullP = do
    _ <- string "null"
    return [Segment "null" (Just $ emptyStyle { color = Just (ANSI 1) })]

jsonSymbolP :: Parser [Segment]
jsonSymbolP = do
    sym <- oneOf ("{}[]:," :: String)
    return [Segment (T.singleton sym) Nothing]

jsonWhitespaceP :: Parser [Segment]
jsonWhitespaceP = do
    ws <- some spaceChar
    return [Segment (T.pack ws) Nothing]

-- Legacy functions for backwards compatibility
highlightJson :: Text -> HRichText
highlightJson input = case parse jsonParser "" input of
    Left _ -> fromPlain input
    Right spans -> HRichText input spans emptyStyle JustifyLeft

highlightHaskell :: Text -> HRichText
highlightHaskell input = case parse haskellParser "" input of
    Left _ -> fromPlain input
    Right spans -> HRichText input spans emptyStyle JustifyLeft

highlightPython :: Text -> HRichText
highlightPython input = case parse pythonParser "" input of
    Left _ -> fromPlain input
    Right spans -> HRichText input spans emptyStyle JustifyLeft

-- Legacy parsers that return spans
jsonParser :: Parser [Span]
jsonParser = return []  -- Simplified for now

haskellParser :: Parser [Span]
haskellParser = return []  -- Simplified for now

pythonParser :: Parser [Span]
pythonParser = return []  -- Simplified for now
