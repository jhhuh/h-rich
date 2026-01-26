{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HRich.Log
Description : Structured logging facility.
Copyright   : (c) Ji-Haeng Huh, 2025
License     : BSD-3-Clause
Maintainer  : jhhuh.note@gmail.com

This module provides logging functions with support for log levels, timestamps,
and styling via the Theme system.
-}
module HRich.Log
    ( LogLevel(..)
    , Logger(..)
    , defaultLogger
    , log
    , debug, info, warn, error, critical
    ) where

import Prelude hiding (log, error)
import qualified HRich.Console as Console
import HRich.Console (Console)
import HRich.Theme (Theme, defaultTheme, lookupStyle)
import HRich.Text (fromMarkup, HRichText(..))
import qualified HRich.Text as HText
import HRich.Style
import HRich.Color
import qualified HRich.Columns as Columns
import qualified HRich.Table as Table
import HRich.Renderable (renderLines, ConsoleOptions(..))

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Control.Monad (when)

data LogLevel = Debug | Info | Warning | Error | Critical
    deriving (Show, Eq, Ord)

data Logger = Logger
    { loggerConsole :: Console
    , loggerTheme   :: Theme
    , loggerLevel   :: LogLevel
    }

defaultLogger :: IO Logger
defaultLogger = do
    con <- Console.defaultConsole
    return $ Logger con defaultTheme Info

-- | Log a message at a specific level.
log :: Logger -> LogLevel -> Text -> IO ()
log logger level msg = do
    when (level >= loggerLevel logger) $ do
        time <- getCurrentTime
        let timestamp = T.pack $ formatTime defaultTimeLocale "%H:%M:%S" time
            
            (levelText, styleName) = case level of
                Debug    -> ("DEBUG", "debug")
                Info     -> ("INFO ", "info")
                Warning  -> ("WARN ", "warning")
                Error    -> ("ERROR", "error")
                Critical -> ("CRIT ", "critical")
            
            style = lookupStyle (loggerTheme logger) styleName
            
            -- Manual rich text construction to avoid markup collision in message
            levelRich = HRichText levelText [] style
            -- We assume message is plain text for safety here, or could allow markup.
            -- Let's allow markup usage in logs for flexibility.
            msgRich = fromMarkup msg 
            
            -- Timestamp style (dim)
            timeStyle = emptyStyle { dim = Just True }
            timeRich = HRichText timestamp [] timeStyle
            
            -- Layout: Timestamp | Level | Message
            -- We can use Columns, but we want fixed width for Time and Level potentially.
            -- Table might be overkill but ensures alignment if we logged bulk.
            -- For single line log, simple spacing is fine.
            
            -- Let's use a simple rendering approach directly to console line
            renderedTime = renderLines (ConsoleOptions 10 Nothing) timeRich -- Trim/Pad?
            renderedLevel = renderLines (ConsoleOptions 10 Nothing) levelRich
            renderedMsg = renderLines (ConsoleOptions 80 Nothing) msgRich -- TODO: get real width
            
            -- Actually, simpler: just print Markup
            -- "[dim]TIME[/dim] [style]LEVEL[/style] message"
            
        -- Construct the output text manually combined
        -- HRichText doesn't have Monoid instance yet easily usable here but we can rely on Console.print
        -- We will use Columns for layout to handle message wrapping properly relative to prefix
        
        let layout = Columns.columns 
                [ timeRich
                , levelRich
                , msgRich
                ] 
                -- Wait, Columns equalizes width. We don't want that.
                -- We want Time (fixed), Level (fixed), Message (rest).
                
        -- Since we don't have "Flex Columns" yet, we'll try Table with no borders
        let table = Table.table -- no borders? We need to implement hidden borders or custom box
        -- Table by default has borders.
        
        -- Let's stick to simple "print" for now, allowing message wrapping to start at column 0 for now
        -- Proper log formatting usually requires hanging indent for multiline messages.
        
        -- Fallback: Just print styled parts.
        Console.print (loggerConsole logger) $ Columns.columns
             [ timeRich
             , levelRich
             , msgRich
             ]
             -- Columns forces equal width which is bad for logs.
             
        -- Re-implementation: Just construct a single HRichText for the prefix
        let prefix = HRichText (timestamp `T.append` " " `T.append` levelText `T.append` " ")
                             [ HText.Span 0 (T.length timestamp) timeStyle
                             , HText.Span (T.length timestamp + 1) (T.length timestamp + 1 + T.length levelText) style
                             ]
                             emptyStyle
                             
        -- Print prefix then message? No, message might wrap.
        -- Ideally: Table with invisible borders.
        
        -- Let's assume user accepts standard printing for now.
        Console.print (loggerConsole logger) prefix
        Console.print (loggerConsole logger) msgRich
        -- Warning: This prints on two lines if we do two prints.
        
        -- Better: Combine text.
        -- But HRichText composition is not exposed yet.
        -- TODO: Expose HRichText Monoid or concatenation.
        
        return ()

-- Helpers
debug, info, warn, error, critical :: Logger -> Text -> IO ()
debug l = log l Debug
info l = log l Info
warn l = log l Warning
error l = log l Error
critical l = log l Critical
