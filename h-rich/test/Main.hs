{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified HRich.Console as Console
import qualified Prelude as P
import Prelude (IO)

main :: IO ()
main = do
    console <- Console.defaultConsole
    
    Console.printMarkup console "Hello [bold red]World[/bold red]!"
    Console.printMarkup console "This is [italic blue]Haskell Rich[/italic blue]."
    Console.printMarkup console "A [on yellow]yellow background[/on yellow] example."
    Console.printMarkup console "[b]Bold [i]and Italic[/i] text[/b]"
    
    -- Test complex nesting and mix of rich/isocline styles
    Console.printMarkup console "[red]Red [green]Green [blue]Blue[/blue] Green[/green] Red[/red]"
    Console.printMarkup console "[u b magenta on white]Underline Bold Magenta on White[/u b magenta on white]"
    
    -- Test escaping
    Console.printMarkup console "Escaped bracket: \\[red]this is not red\\[/red]"
    
    P.putStrLn "Test complete."
