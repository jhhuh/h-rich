{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HRich.Console
Description : Console output interface.
Copyright   : (c) Ji-Haeng Huh, 2025
License     : BSD-3-Clause
Maintainer  : jhhuh.note@gmail.com

This module provides the main entry points for printing renderable components
to the terminal. It handles terminal width detection and low-level I/O.
-}
module HRich.Console
    ( -- * Console Handle
      Console(..)
    , defaultConsole
      -- * Printing
    , print
    , printMarkup
    ) where

import Prelude hiding (print)
import HRich.Text
import HRich.Segment
import HRich.Renderable
import System.IO (Handle, stdout, hFlush, hIsTerminalDevice)
import Data.Text (Text)
import qualified Data.Text.IO as TIO

import System.Console.Terminal.Size (size, Window(..))
import qualified System.Console.Terminal.Size as TS

data Console = Console
    { consoleHandle :: Handle
    , isTerminal    :: Bool
    }

defaultConsole :: IO Console
defaultConsole = do
    isTerm <- hIsTerminalDevice stdout
    return $ Console stdout isTerm

print :: Renderable a => Console -> a -> IO ()
print console r = do
    s <- size :: IO (Maybe (Window Int))
    let consoleW = maybe 80 TS.width s
        options = ConsoleOptions consoleW Nothing
        lines' = renderLines options r
    mapM_ (\line -> do
        mapM_ (TIO.hPutStr (consoleHandle console) . renderSegment) line
        TIO.hPutStr (consoleHandle console) "\n") lines'
    hFlush (consoleHandle console)

printMarkup :: Console -> Text -> IO ()
printMarkup console markup = print console (fromMarkup markup)
