{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified HRich.Console as Console
import qualified HRich.Text as Text
import Data.Text (Text)
import qualified Prelude as P
import Prelude (IO)

main :: IO ()
main = do
    console <- Console.defaultConsole
    
    Console.printMarkup console "Hello [bold red]World[/bold red]!"
    Console.printMarkup console "This is [italic blue]Haskell Rich[/italic blue]."
    Console.printMarkup console "A [on yellow black]styled background[/on yellow black] example."
    Console.printMarkup console "[bold]Bold [italic]and Italic[/italic] text[/bold]"
    
    -- Test complex nesting
    Console.printMarkup console "[red]Red [green]Green [blue]Blue[/blue] Green[/green] Red[/red]"
    
    P.putStrLn "Test complete."
