{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified HRich.Console as Console
import qualified HRich.Panel as Panel
import qualified HRich.Columns as Columns
import qualified HRich.Table as Table
import qualified HRich.Progress as Progress
import qualified HRich.Text as Text
import qualified Prelude as P
import Prelude (IO, ($), (.), id, return)

main :: IO ()
main = do
    console <- Console.defaultConsole
    
    let header = Panel.panel (Text.fromMarkup "[bold white on blue] h-rich [/bold white on blue]")
    
    let intro = Panel.panel (Text.fromMarkup "A Haskell port of the [bold magenta]Rich[/bold magenta] library for Python.\nSupports [green]colors[/green], [italic]styles[/italic], tables, progress bars, and more.")
    
    let styles = Panel.panel (Text.fromMarkup "[bold]Bold[/bold]\n[italic]Italic[/italic]\n[underline]Underline[/underline]\n[strike]Strike[/strike]")
    
    let colors = Panel.panel (Text.fromMarkup "[red]Red[/red] [green]Green[/green] [blue]Blue[/blue]\n[yellow]Yellow[/yellow] [magenta]Magenta[/magenta] [cyan]Cyan[/cyan]\n[rgb(255,165,0)]TrueColor Orange[/rgb(255,165,0)]")
    
    let movieTable = Table.addRow ["Star Wars: A New Hope", "1977", "$775M"]
                   . Table.addRow ["The Empire Strikes Back", "1980", "$538M"]
                   . Table.addRow ["Return of the Jedi", "1983", "$475M"]
                   . Table.addColumn "Revenue"
                   . Table.addColumn "Year"
                   . Table.addColumn "Title"
                   $ Table.table

    let progress = (Progress.progressBar 0.75 1.0) { Progress.progressLabel = P.Just "Downloading assets..." }
    
    let layout = Columns.columns 
            [ Panel.panel (Text.fromMarkup "[yellow]Styles & Colors[/yellow]\n\nSee the panel below.")
            , Panel.panel (Text.fromMarkup "[cyan]Layouts[/cyan]\n\nResponsive grids.")
            ]

    let dashboard = Panel.panel $ Columns.columns [styles, colors]

    Console.print console header
    Console.print console intro
    Console.print console layout
    Console.print console dashboard
    Console.print console movieTable
    Console.print console (Panel.panel progress)
