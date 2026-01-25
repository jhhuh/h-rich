{-# LANGUAGE OverloadedStrings #-}
module HRich.Console
    ( Console(..)
    , defaultConsole
    , print
    , printMarkup
    ) where

import Prelude hiding (print)
import HRich.Text
import HRich.Segment
import System.IO (Handle, stdout, hFlush, hIsTerminalDevice)
import Data.Text (Text)
import qualified Data.Text.IO as TIO

data Console = Console
    { consoleHandle :: Handle
    , isTerminal    :: Bool
    }

defaultConsole :: IO Console
defaultConsole = do
    isTerm <- hIsTerminalDevice stdout
    return $ Console stdout isTerm

print :: Console -> HRichText -> IO ()
print console t = do
    let segments = renderText t
    mapM_ (TIO.hPutStr (consoleHandle console) . renderSegment) segments
    TIO.hPutStr (consoleHandle console) "\n"
    hFlush (consoleHandle console)

printMarkup :: Console -> Text -> IO ()
printMarkup console markup = print console (fromMarkup markup)
