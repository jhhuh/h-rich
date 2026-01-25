{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified HRich.Console as Console
import qualified HRich.Panel as Panel
import qualified HRich.Columns as Columns
import qualified HRich.Table as Table
import qualified HRich.Progress as Progress
import qualified HRich.Text as Text
import qualified Prelude as P
import Prelude (IO, ($), (.), id)

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
    
    -- Test Panel
    Console.print console (Panel.panel (Text.fromMarkup "This is a [bold]Panel[/bold] content!"))
    
    -- Test multi-line Panel
    Console.print console (Panel.panel (Text.fromMarkup "Line 1: [red]Red[/red]\nLine 2: [green]Green[/green]\nLine 3: [blue]Blue[/blue]"))
    
    -- Test wrapping
    Console.print console (Text.fromMarkup "[magenta]This is a very long line that should be wrapped if we specify a smaller width in options.[/magenta]")
    
    -- Test nested Panels
    let innerPanel = Panel.panel (Text.fromMarkup "Inner Panel [red]Content[/red]")
        outerPanel = Panel.panel innerPanel
    Console.print console outerPanel
    
    -- Test Columns
    let col1 = Panel.panel (Text.fromMarkup "[yellow]Left[/yellow] Column content that wraps nicely.")
        col2 = Panel.panel (Text.fromMarkup "[cyan]Right[/cyan] Column with [bold]bold[/bold] text.")
    Console.print console (Columns.columns [col1, col2])
    
    -- Final Showcase
    let finalContent = Columns.columns 
            [ Panel.panel (Text.fromMarkup "Col 1\n[red]Nix[/red]")
            , Panel.panel (Text.fromMarkup "Col 2\n[green]Haskell[/green]")
            , Panel.panel (Text.fromMarkup "Col 3\n[blue]Rich[/blue]")
            ]
    Console.print console (Panel.panel finalContent)

    -- Test Table
    let table' = Table.addRow ["HLS", "[red]Crashed[/red]"]
               . Table.addRow ["Cabal", "[yellow]Building[/yellow]"]
               . Table.addRow ["GHC", "[green]Running[/green]"]
               . Table.addColumn "Status"
               . Table.addColumn "Name"
               $ Table.table
    Console.print console table'

    -- Test Progress
    let progress = (Progress.progressBar 45 100) { Progress.progressLabel = P.Just "Downloading..." }
    Console.print console progress

    P.putStrLn "Test complete."
