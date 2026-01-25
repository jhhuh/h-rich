{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HRich.Markdown
Description : Basic Markdown rendering.
Copyright   : (c) Ji-Haeng Huh, 2025
License     : BSD-3-Clause
Maintainer  : jhhuh.note@gmail.com

This module provides basic Markdown parsing and rendering capabilities.
It supports headers, bold/italic text, and code blocks.
-}
module HRich.Markdown
    ( renderMarkdown
    ) where

import HRich.Text
import HRich.Renderable
import HRich.Style
import HRich.Color
import HRich.Segment (Segment(..))
import HRich.Panel
import HRich.Box
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

type Parser = Parsec Void Text

-- | Render a Markdown string into a list of Renderable components.
-- Note: Since our Renderable system effectively produces lines of Segments, 
-- and we want to return a composite object, we'll return a composite wrapper.
data MarkdownDoc = MarkdownDoc [MarkdownBlock]

data MarkdownBlock 
    = Header Int HRichText
    | Paragraph HRichText
    | CodeBlock Text -- language ignore for now
    | List [HRichText]
    deriving (Show)

instance Renderable MarkdownDoc where
    render opts (MarkdownDoc blocks) = concatMap (render opts) blocks
    renderLines opts (MarkdownDoc blocks) = concatMap (renderLines opts) blocks

instance Renderable MarkdownBlock where
    render opts block = concat (renderLines opts block)
    
    renderLines opts (Header level text) =
        let style = case level of
                1 -> emptyStyle { bold = Just True, color = Just (ANSI 5), underline = Just True } -- Magenta Underlined
                2 -> emptyStyle { bold = Just True, color = Just (ANSI 4) } -- Blue Bold
                _ -> emptyStyle { bold = Just True }
            segments = renderLines opts (applyStyle style text)
        in segments ++ [[Segment "" Nothing]] -- Add spacing
        
    renderLines opts (Paragraph text) =
        renderLines opts text ++ [[Segment "" Nothing]]
        
    renderLines opts (CodeBlock content) =
        let p = Panel 
                { panelRenderable = fromPlain content
                , panelTitle = Nothing
                , panelBox = square
                , panelStyle = emptyStyle { color = Just (ANSI 2) } -- Green content
                , panelExpand = True
                }
        in renderLines opts p ++ [[Segment "" Nothing]]
        
    renderLines opts (List items) =
        concatMap renderItem items ++ [[Segment "" Nothing]]
      where
        renderItem item = 
            let lines' = renderLines (opts { consoleWidth = consoleWidth opts - 2 }) item
                bullet = Segment "• " (Just (emptyStyle { color = Just (ANSI 3) })) -- Yellow bullet
            in case lines' of
                [] -> []
                (first:rest) -> (bullet : first) : map (Segment "  " Nothing :) rest

-- | Main entry point
renderMarkdown :: Text -> MarkdownDoc
renderMarkdown input = 
    case parse markdownParser "" input of
        Left _ -> MarkdownDoc [Paragraph (fromPlain input)] -- Fallback
        Right blocks -> MarkdownDoc blocks

markdownParser :: Parser [MarkdownBlock]
markdownParser = many blockP

blockP :: Parser MarkdownBlock
blockP = try headerP <|> try codeBlockP <|> try listP <|> paragraphP

headerP :: Parser MarkdownBlock
headerP = do
    hashes <- some (char '#')
    _ <- spaceChar
    content <- takeWhileP Nothing (/= '\n')
    _ <- optional newline
    return $ Header (length hashes) (fromPlain content) -- Rich text parsing inside headers TODO

codeBlockP :: Parser MarkdownBlock
codeBlockP = do
    _ <- string "```"
    _ <- optional (many (noneOf ['\n'])) -- language
    _ <- newline
    content <- manyTill anySingle (string "```")
    _ <- optional newline
    return $ CodeBlock (T.pack content)

listP :: Parser MarkdownBlock
listP = do
    items <- some itemP
    return $ List items
  where
    itemP = do
        _ <- string "- " <|> string "* "
        content <- takeWhileP Nothing (/= '\n')
        _ <- optional newline
        return $ fromPlain content

paragraphP :: Parser MarkdownBlock
paragraphP = do
    content <- takeWhile1P Nothing (/= '\n')
    _ <- optional newline
    return $ Paragraph (fromPlain content) -- TODO: Parse styled text

applyStyle :: Style -> HRichText -> HRichText
applyStyle s (HRichText t spans base) = HRichText t spans (combineStyles base s)
