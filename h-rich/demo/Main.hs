{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified HRich.Console as Console
import qualified HRich.Panel as Panel
import qualified HRich.Table as Table
import qualified HRich.Progress as Progress
import qualified HRich.Tree as Tree
import qualified HRich.Syntax as Syntax
import qualified HRich.Markdown as Markdown
import qualified HRich.Text as Text
import HRich.Text (HRichText)
import HRich.Segment (Segment(..))
import HRich.Style (Style(..), emptyStyle)
import HRich.Color (Color(..))
import HRich.Renderable (Renderable(..), ConsoleOptions(..))
import HRich.Box (rounded)
import qualified Data.Text as T
import Data.Word (Word8)

-- | Generate a color spectrum using HSL to RGB conversion
-- Uses the "▄" character with different fg/bg colors for smooth gradient
newtype ColorBox = ColorBox Int

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

-- | Create a comparison grid (two items side by side)
comparison :: HRichText -> HRichText -> Table.Table
comparison left right =
    Table.addRichRow [left, right]
    $ Table.grid

main :: IO ()
main = do
    console <- Console.defaultConsole

    -- Main feature table (grid layout like Rich)
    let featureTable = makeFeatureTable

    -- Title
    Console.printMarkup console "[bold magenta]h-rich[/bold magenta] features\n"

    -- Print the feature table
    Console.print console featureTable

    -- Closing panel
    let closingPanel = Panel.Panel
            { Panel.panelRenderable = Text.fromMarkup "[bold magenta]Thanks for trying h-rich![/bold magenta]\n\nA Haskell port of Python's Rich library.\n[cyan]https://github.com/jhhuh/h-rich[/cyan]"
            , Panel.panelTitle = Just "h-rich"
            , Panel.panelBox = rounded
            , Panel.panelStyle = emptyStyle { color = Just (RGB 0 255 0) }
            , Panel.panelExpand = True
            }
    Console.print console closingPanel

-- | Create the main feature demonstration table
makeFeatureTable :: Table.Table
makeFeatureTable =
    Table.addRichRow [label "+more!", moreDemo]
    $ Table.addRichRow [label "Progress", progressDemo]
    $ Table.addRichRow [label "Tree", treeDemo]
    $ Table.addRichRow [label "Markdown", markdownDemo]
    $ Table.addRichRow [label "Syntax\nhighlighting", syntaxDemo]
    $ Table.addRichRow [label "Tables", tablesDemo]
    $ Table.addRichRow [label "Markup", markupDemo]
    $ Table.addRichRow [label "Text", textDemo]
    $ Table.addRichRow [label "Styles", stylesDemo]
    $ Table.addRichRow [label "Colors", colorsDemo]
    $ Table.grid { Table.tablePadding = 2 }
  where
    label :: T.Text -> HRichText
    label t = Text.fromMarkup $ "[bold red]" `T.append` t `T.append` "[/bold red]"

-- | Colors demonstration
colorsDemo :: HRichText
colorsDemo = Text.fromMarkup $
    "[green]✓[/green] [bold green]4-bit color[/bold green]\n" `T.append`
    "[green]✓[/green] [bold blue]8-bit color[/bold blue]\n" `T.append`
    "[green]✓[/green] [bold magenta]Truecolor (16.7 million)[/bold magenta]\n" `T.append`
    "[green]✓[/green] [bold cyan]Automatic color conversion[/bold cyan]"

-- | Styles demonstration
stylesDemo :: HRichText
stylesDemo = Text.fromMarkup
    "All ANSI styles: [bold]bold[/bold], [dim]dim[/dim], [italic]italic[/italic], [underline]underline[/underline], [strike]strikethrough[/strike], [reverse]reverse[/reverse], and [blink]blink[/blink]."

-- | Text justification demonstration
textDemo :: HRichText
textDemo = Text.fromMarkup
    "Word wrap text. Justify [green]left[/green], [yellow]center[/yellow], [blue]right[/blue], or [red]full[/red]."

-- | Markup demonstration
markupDemo :: HRichText
markupDemo = Text.fromMarkup
    "[bold magenta]Rich[/bold magenta] supports a simple [italic]bbcode[/italic]-like [bold]markup[/bold] for [yellow]color[/yellow], [underline]style[/underline], and more!"

-- | Tables demonstration (rendered as text since we can't nest tables easily)
tablesDemo :: HRichText
tablesDemo = Text.fromMarkup $
    "[green]Date[/green]         [blue]Title[/blue]                            [cyan]Budget[/cyan]        [magenta]Box Office[/magenta]\n" `T.append`
    "Dec 20, 2019   Star Wars: Rise of Skywalker     $275,000,000  [bold]$375,126,118[/bold]\n" `T.append`
    "May 25, 2018   [bold]Solo[/bold]: A Star Wars Story         $275,000,000  $393,151,347\n" `T.append`
    "Dec 15, 2017   Star Wars Ep. VIII: Last Jedi   $262,000,000  [bold cyan]$1,332,539,889[/bold cyan]\n" `T.append`
    "May 19, 1999   Star Wars Ep. [bold]I[/bold]: Phantom Menace  $115,000,000  $1,027,044,677"

-- | Syntax highlighting demonstration
syntaxDemo :: HRichText
syntaxDemo = Text.fromMarkup $
    "[dim]-- Haskell code with syntax highlighting[/dim]\n" `T.append`
    "[magenta]iterLast[/magenta] :: [blue]Traversable[/blue] t => t a -> t ([blue]Bool[/blue], a)\n" `T.append`
    "[magenta]iterLast[/magenta] xs = [magenta]snd[/magenta] $ [magenta]mapAccumR[/magenta] step [yellow]True[/yellow] xs\n" `T.append`
    "  [magenta]where[/magenta]\n" `T.append`
    "    step isLast x = ([yellow]False[/yellow], (isLast, x))"

-- | Markdown demonstration
markdownDemo :: HRichText
markdownDemo = Text.fromMarkup $
    "[dim]# Markdown[/dim]\n" `T.append`
    "Supports *markdown* syntax!\n" `T.append`
    "- Headers\n" `T.append`
    "- Basic formatting: [bold]bold[/bold], [italic]italic[/italic], [cyan]`code`[/cyan]\n" `T.append`
    "- Block quotes\n" `T.append`
    "- Lists, and more..."

-- | Tree demonstration
treeDemo :: HRichText
treeDemo = Text.fromMarkup $
    "[bold cyan]src[/bold cyan]\n" `T.append`
    "├── [yellow]HRich[/yellow]\n" `T.append`
    "│   ├── Console.hs\n" `T.append`
    "│   ├── Text.hs\n" `T.append`
    "│   └── [green]Style.hs[/green]\n" `T.append`
    "└── [yellow]demo[/yellow]\n" `T.append`
    "    └── [magenta]Main.hs[/magenta]"

-- | Progress bar demonstration
progressDemo :: HRichText
progressDemo = Text.fromMarkup $
    "Installing... \\[[green]━━━━━━━━━━━━━━━━━━━━━━━━━[/green][dim]━━━━━━━━━━[/dim]\\] 75%"

-- | +more demonstration
moreDemo :: HRichText
moreDemo = Text.fromMarkup
    "Columns, panels, logging, tracebacks, themes, prompts, and more..."
