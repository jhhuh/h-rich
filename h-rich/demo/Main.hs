{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified HRich.Console as Console
import qualified HRich.Panel as Panel
import qualified HRich.Columns as Columns
import qualified HRich.Table as Table
import qualified HRich.Progress as Progress
import qualified HRich.Tree as Tree
import qualified HRich.Syntax as Syntax
import qualified HRich.Markdown as Markdown
import qualified HRich.Text as Text
import HRich.Segment (Segment(..))
import HRich.Style (Style(..), emptyStyle)
import HRich.Color (Color(..))
import HRich.Renderable (Renderable(..), ConsoleOptions(..))
import HRich.Box (rounded)
import qualified Data.Text as T
import Data.Word (Word8)

-- | Generate a color spectrum using HSL to RGB conversion
-- Uses the "▄" character with different fg/bg colors for smooth gradient
newtype ColorBox = ColorBox Int  -- width

hslToRgb :: Double -> Double -> Double -> (Word8, Word8, Word8)
hslToRgb h s l =
    let c = (1 - abs (2 * l - 1)) * s
        x = c * (1 - abs (((h / 60) `mod'` 2) - 1))
        m = l - c / 2
        (r', g', b') = case floor (h / 60) `mod` 6 of
            0 -> (c, x, 0)
            1 -> (x, c, 0)
            2 -> (0, c, x)
            3 -> (0, x, c)
            4 -> (x, 0, c)
            _ -> (c, 0, x)
        toWord8 v = round ((v + m) * 255)
    in (toWord8 r', toWord8 g', toWord8 b')
  where
    mod' a b = a - b * fromIntegral (floor (a / b) :: Int)

instance Renderable ColorBox where
    render options (ColorBox _) = concat (renderLines options (ColorBox 0))

    renderLines options (ColorBox _) =
        let width = consoleWidth options
            rows = 5 :: Int
            makeRow y =
                [ let h = (fromIntegral x / fromIntegral width) * 360
                      l1 = 0.1 + (fromIntegral y / fromIntegral rows) * 0.7
                      l2 = l1 + 0.07
                      (r1, g1, b1) = hslToRgb h 1.0 l1
                      (r2, g2, b2) = hslToRgb h 1.0 l2
                      style = emptyStyle { color = Just (RGB r2 g2 b2), bgColor = Just (RGB r1 g1 b1) }
                  in Segment "▄" (Just style)
                | x <- [0..width-1]
                ]
        in [ makeRow y | y <- [0..rows-1] ]

main :: IO ()
main = do
    console <- Console.defaultConsole

    -- Title
    Console.printMarkup console "[bold magenta]h-rich[/bold magenta] [dim]features[/dim]\n"

    -- Colors section
    Console.printMarkup console "\n[bold red]Colors[/bold red]"
    Console.printMarkup console "  [green]✓[/green] [bold green]4-bit color[/bold green]"
    Console.printMarkup console "  [green]✓[/green] [bold blue]8-bit color[/bold blue]"
    Console.printMarkup console "  [green]✓[/green] [bold magenta]Truecolor (16.7 million)[/bold magenta]"
    Console.printMarkup console "  [green]✓[/green] [bold cyan]Automatic color conversion[/bold cyan]\n"
    Console.print console (ColorBox 80)

    -- Styles section
    Console.printMarkup console "\n[bold red]Styles[/bold red]"
    Console.printMarkup console "  All ANSI styles: [bold]bold[/bold], [dim]dim[/dim], [italic]italic[/italic], [underline]underline[/underline], [strike]strikethrough[/strike], and [blink]blink[/blink].\n"

    -- Text justification section
    Console.printMarkup console "[bold red]Text[/bold red]"
    Console.printMarkup console "  Word wrap text. Justify [green]left[/green], [yellow]center[/yellow], [blue]right[/blue], or [red]full[/red].\n"
    let lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
    Console.print console $ Columns.columns
        [ Text.leftJustify $ Text.fromMarkup $ "[green]" `T.append` lorem `T.append` "[/green]"
        , Text.centerJustify $ Text.fromMarkup $ "[yellow]" `T.append` lorem `T.append` "[/yellow]"
        , Text.rightJustify $ Text.fromMarkup $ "[blue]" `T.append` lorem `T.append` "[/blue]"
        ]

    -- Markup section
    Console.printMarkup console "[bold red]Markup[/bold red]"
    Console.printMarkup console "  [bold magenta]Rich[/bold magenta] supports a simple [italic]bbcode[/italic]-like [bold]markup[/bold] for [yellow]color[/yellow], [underline]style[/underline], and more!\n"

    -- Tables section
    Console.printMarkup console "[bold red]Tables[/bold red]"
    let movieTable = Table.addRow ["Star Wars: The Rise of Skywalker", "Dec 20, 2019", "$275,000,000", "[bold]$375,126,118[/bold]"]
                   . Table.addRow ["[bold]Solo[/bold]: A Star Wars Story", "May 25, 2018", "$275,000,000", "$393,151,347"]
                   . Table.addRow ["Star Wars Ep. VIII: The Last Jedi", "Dec 15, 2017", "$262,000,000", "[bold cyan]$1,332,539,889[/bold cyan]"]
                   . Table.addRow ["Star Wars Ep. [bold]I[/bold]: The Phantom Menace", "May 19, 1999", "$115,000,000", "$1,027,044,677"]
                   . Table.addColumn "[magenta]Box Office[/magenta]"
                   . Table.addColumn "[cyan]Budget[/cyan]"
                   . Table.addColumn "[green]Date[/green]"
                   . Table.addColumn "[blue]Title[/blue]"
                   $ Table.table
    Console.print console movieTable

    -- Syntax highlighting section
    Console.printMarkup console "\n[bold red]Syntax Highlighting[/bold red]"
    let jsonSource = "{\n  \"name\": \"h-rich\",\n  \"version\": \"0.1.0\",\n  \"haskell\": true,\n  \"features\": null\n}"
    Console.print console (Panel.panel (Syntax.highlightJson jsonSource))

    -- Tree section
    Console.printMarkup console "\n[bold red]Tree[/bold red]"
    let treeNode = Tree.Node (Text.fromMarkup "[bold cyan]src[/bold cyan]")
            [ Tree.Node (Text.fromMarkup "[yellow]HRich[/yellow]")
                [ Tree.Node (Text.fromMarkup "Console.hs") []
                , Tree.Node (Text.fromMarkup "Text.hs") []
                , Tree.Node (Text.fromMarkup "[green]Style.hs[/green]") []
                ]
            , Tree.Node (Text.fromMarkup "[yellow]demo[/yellow]")
                [ Tree.Node (Text.fromMarkup "[magenta]Main.hs[/magenta]") []
                ]
            ]
    Console.print console (Tree.Tree treeNode)

    -- Markdown section
    Console.printMarkup console "\n[bold red]Markdown[/bold red]"
    let mdSource = "# h-rich\n\nSupports *markdown* rendering with **bold**, *italic*, and `code`."
    Console.print console (Panel.panel (Markdown.renderMarkdown mdSource))

    -- Progress section
    Console.printMarkup console "\n[bold red]Progress[/bold red]"
    let progress = (Progress.progressBar 0.75 1.0) { Progress.progressLabel = Just "Installing dependencies..." }
    Console.print console (Panel.panel progress)

    -- Footer
    Console.printMarkup console "\n[bold red]+more![/bold red]"
    Console.printMarkup console "  Columns, logging, tracebacks, themes, and more...\n"

    -- Closing panel
    let closingPanel = Panel.Panel
            { Panel.panelRenderable = Text.fromMarkup "[bold magenta]Thanks for trying h-rich![/bold magenta]\n\nA Haskell port of Python's Rich library.\n[cyan]https://github.com/jhhuh/h-rich[/cyan]"
            , Panel.panelTitle = Just "h-rich"
            , Panel.panelBox = rounded
            , Panel.panelStyle = emptyStyle { color = Just (RGB 0 255 0) }
            , Panel.panelExpand = True
            }
    Console.print console closingPanel
