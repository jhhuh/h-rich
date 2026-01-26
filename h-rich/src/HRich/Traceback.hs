{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HRich.Traceback
Description : Exception rendering.
Copyright   : (c) Ji-Haeng Huh, 2025
License     : BSD-3-Clause
Maintainer  : jhhuh.note@gmail.com

This module provides facilities to catch and render exceptions in a rich,
readable format, mimicking Python's Rich tracebacks.
-}
module HRich.Traceback
    ( printException
    , withTraceback
    ) where

import Control.Exception (SomeException, displayException, catch)
import qualified Data.Text as T
import qualified HRich.Console as Console
import qualified System.Exit
import HRich.Console (Console)
import HRich.Panel
import HRich.Text (fromMarkup)
import HRich.Style
import HRich.Color
import HRich.Box (heavy)

-- | Print a 'SomeException' to the console with rich formatting.
printException :: Console -> SomeException -> IO ()
printException console e = do
    let msg = T.pack $ displayException e
        -- We can try to parse the message to see if it has location info,
        -- but for now, just wrapping it in a red panel is a good start.
        
        -- Style for the error message
        errorStyle = emptyStyle { color = Just (ANSI 1) } -- Red
        
        -- Header text
        header = "[bold red]Traceback (Most recent call last)[/bold red]"
        
        -- Content text (the exception)
        content = fromMarkup ("[red]" `T.append` msg `T.append` "[/red]")
        
        -- Create the panel
        p = Panel 
            { panelRenderable = content
            , panelTitle = Just (T.pack "Exception Occurred")
            , panelBox = heavy
            , panelStyle = errorStyle
            , panelExpand = True
            }
            
    Console.printMarkup console header
    Console.print console p

-- | Run an action and print any exception that occurs with rich formatting.
withTraceback :: IO a -> IO a
withTraceback action = do
    console <- Console.defaultConsole
    catch action (\e -> do
        printException console e
        _ <- error "Program exited due to unhandled exception" -- Re-throw or exit? 
        -- Usually we want to exit gracefully or re-throw.
        -- Let's re-throw for now, but after printing.
        -- Actually, catch catches it, so we need to decide.
        -- Users might want to suppress the default handler.
        System.Exit.exitFailure
        )
