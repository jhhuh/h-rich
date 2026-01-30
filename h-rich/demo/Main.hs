{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified HRich.Console as Console
import qualified HRich.Panel as Panel
import qualified HRich.Table as Table
import qualified HRich.Syntax as Syntax
import qualified HRich.Text as Text
import qualified HRich.Columns as Columns
import qualified HRich.Markdown as Markdown
import HRich.Segment (Segment(..))
import HRich.Style (Style(..), emptyStyle)
import HRich.Color (Color(..))
import HRich.Renderable (Renderable(..), ConsoleOptions(..), Indented(..))
import HRich.Box (rounded, simple)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Word (Word8)

-- | Color spectrum box using HSL to RGB conversion
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

-- | Padding spaces for continuation lines (14 chars to match labels)
pad :: T.Text
pad = "              "

main :: IO ()
main = do
    console <- Console.defaultConsole

    -- Title
    Console.print console (Text.centerJustify (Text.fromMarkup "[bold magenta]h-rich[/bold magenta] features"))
    TIO.putStrLn "\n"

    -- Colors section with spectrum
    Console.printMarkup console "[bold red]Colors[/bold red]\n"
    Console.print console (Indented 14 (ColorBox 0))
    Console.printMarkup console (pad `T.append` "[green]✓[/green] [bold green]4-bit color[/bold green]  [green]✓[/green] [bold blue]8-bit color[/bold blue]  [green]✓[/green] [bold magenta]Truecolor (16.7 million)[/bold magenta]  [green]✓[/green] [bold cyan]Auto convert[/bold cyan]\n\n")

    -- Styles (removed blink from demo to prevent animation in screenshot)
    Console.printMarkup console "[bold red]Styles[/bold red]        All ANSI styles: [bold]bold[/bold], [dim]dim[/dim], [italic]italic[/italic], [underline]underline[/underline], [strike]strikethrough[/strike], [reverse]reverse[/reverse], and blink.\n"

    -- Text justification demo
    Console.printMarkup console "[bold red]Text[/bold red]\n"
    Console.print console (Indented 14 makeJustifyDemo)
    TIO.putStrLn ""

    -- Markup
    Console.printMarkup console "[bold red]Markup[/bold red]        [bold magenta]Rich[/bold magenta] supports a simple [italic]bbcode[/italic]-like [bold]markup[/bold] for [yellow]color[/yellow], [underline]style[/underline], and more!\n"

    -- CJK/Emoji
    Console.printMarkup console "[bold red]CJK/Emoji[/bold red]     [bold cyan]中文[/bold cyan] [bold green]日本語[/bold green] [bold yellow]한국어[/bold yellow] properly aligned!\n"
    Console.printMarkup console (pad `T.append` "Wide chars: 你好世界 | Emoji: ✓ ✗ ★ ♥ ● ■\n\n")

    -- Tables - Star Wars filmography with simple h-line style
    Console.printMarkup console "[bold red]Tables[/bold red]\n"
    Console.print console (Indented 14 makeStarWarsTable)
    TIO.putStrLn ""

    -- Syntax highlighting + Pretty printing side by side
    Console.printMarkup console "[bold red]Syntax[/bold red]\n"
    Console.print console (Indented 14 makeSyntaxAndJsonDemo)
    TIO.putStrLn ""

    -- Markdown: raw vs rendered
    Console.printMarkup console "[bold red]Markdown[/bold red]\n"
    Console.print console (Indented 14 makeMarkdownDemo)
    TIO.putStrLn ""

    -- Tree with deeper nesting
    Console.printMarkup console "[bold red]Tree[/bold red]          [bold cyan]project[/bold cyan]\n"
    Console.printMarkup console (pad `T.append` "├── [yellow]src[/yellow]\n")
    Console.printMarkup console (pad `T.append` "│   ├── [yellow]HRich[/yellow]\n")
    Console.printMarkup console (pad `T.append` "│   │   ├── Console.hs\n")
    Console.printMarkup console (pad `T.append` "│   │   ├── [green]Style.hs[/green]\n")
    Console.printMarkup console (pad `T.append` "│   │   └── [yellow]Internal[/yellow]\n")
    Console.printMarkup console (pad `T.append` "│   │       └── Utils.hs\n")
    Console.printMarkup console (pad `T.append` "│   └── Main.hs\n")
    Console.printMarkup console (pad `T.append` "├── [yellow]test[/yellow]\n")
    Console.printMarkup console (pad `T.append` "│   └── [magenta]Spec.hs[/magenta]\n")
    Console.printMarkup console (pad `T.append` "└── README.md\n\n")

    -- Progress
    Console.printMarkup console "[bold red]Progress[/bold red]      Installing... \\[[green]━━━━━━━━━━━━━━━━━━━━━━━━━[/green][dim]━━━━━━━━━━[/dim]\\] 75%\n\n"

    -- Columns
    Console.printMarkup console "[bold red]Columns[/bold red]\n"
    Console.print console (Indented 14 makeColumnsDemo)
    TIO.putStrLn ""

    -- +more
    Console.printMarkup console "[bold red]+more![/bold red]        Columns, panels, logging, tracebacks, themes, prompts, and more...\n\n"

    -- Closing panel
    let closingPanel = Panel.Panel
            { Panel.panelRenderable = Text.fromMarkup "[bold magenta]Thanks for trying h-rich![/bold magenta]\n\nA Haskell port of Python's Rich library.\n[cyan]https://github.com/jhhuh/h-rich[/cyan]"
            , Panel.panelTitle = Just "h-rich"
            , Panel.panelBox = rounded
            , Panel.panelStyle = emptyStyle { color = Just (RGB 0 255 0) }
            , Panel.panelExpand = True
            }
    Console.print console closingPanel

-- | Create the Star Wars movie table with simple h-line style
makeStarWarsTable :: Table.Table
makeStarWarsTable =
    Table.addRichRow [Text.fromMarkup "[green]Dec 20, 2019[/green]", Text.fromMarkup "[blue]Star Wars: Rise of Skywalker[/blue]", Text.fromMarkup "[cyan]$275,000,000[/cyan]", Text.fromMarkup "[magenta]$375,126,118[/magenta]"]
    $ Table.addRichRow [Text.fromMarkup "[green]May 25, 2018[/green]", Text.fromMarkup "[blue][bold]Solo[/bold]: A Star Wars Story[/blue]", Text.fromMarkup "[cyan]$275,000,000[/cyan]", Text.fromMarkup "[magenta]$393,151,347[/magenta]"]
    $ Table.addRichRow [Text.fromMarkup "[green]Dec 15, 2017[/green]", Text.fromMarkup "[blue]Star Wars Ep. VIII: Last Jedi[/blue]", Text.fromMarkup "[cyan]$262,000,000[/cyan]", Text.fromMarkup "[magenta][bold]$1,332,539,889[/bold][/magenta]"]
    $ Table.addRichRow [Text.fromMarkup "[green]May 19, 1999[/green]", Text.fromMarkup "[blue]Star Wars Ep. [bold]I[/bold]: Phantom Menace[/blue]", Text.fromMarkup "[cyan]$115,000,000[/cyan]", Text.fromMarkup "[magenta]$1,027,044,677[/magenta]"]
    $ Table.addColumn "[green]Released[/green]"
    $ Table.addColumn "[blue]Title[/blue]"
    $ Table.addColumn "[cyan]Budget[/cyan]"
    $ Table.addColumn "[magenta]Box Office[/magenta]"
    $ Table.table { Table.tableBox = simple }

-- | Create the syntax highlighting + JSON demo side by side
makeSyntaxAndJsonDemo :: Columns.Columns
makeSyntaxAndJsonDemo =
    let haskellCode = T.unlines
            [ "data User = User"
            , "    { name :: Text"
            , "    , age  :: Int"
            , "    } deriving Show"
            , ""
            , "greet :: User -> Text"
            , "greet user ="
            , "    \"Hello, \" <> name user"
            ]
        syntaxBlock = (Syntax.syntax haskellCode "haskell")
            { Syntax.syntaxLineNumbers = True
            , Syntax.syntaxBgColor = Just (RGB 253 246 227)  -- Solarized Light
            , Syntax.syntaxTheme = Syntax.lightTheme
            }
        jsonCode = T.unlines
            [ "{"
            , "    \"name\": \"Alice\","
            , "    \"age\": 30,"
            , "    \"active\": true"
            , "}"
            ]
        jsonBlock = (Syntax.syntax jsonCode "json")
            { Syntax.syntaxLineNumbers = True
            , Syntax.syntaxBgColor = Just (RGB 40 42 54)  -- Dark background
            , Syntax.syntaxTheme = Syntax.darkTheme
            }
    in Columns.columns [syntaxBlock, jsonBlock]

-- | Create the markdown raw vs rendered demo
makeMarkdownDemo :: Columns.Columns
makeMarkdownDemo =
    let markdownSource = T.unlines
            [ "# Heading"
            , "## Subheading"
            , "- First item"
            , "- Second item"
            ]
        rawPanel = Panel.Panel
            { Panel.panelRenderable = Text.fromPlain markdownSource
            , Panel.panelTitle = Just "Markdown"
            , Panel.panelBox = rounded
            , Panel.panelStyle = emptyStyle { color = Just (RGB 150 150 150) }
            , Panel.panelExpand = True
            }
        renderedPanel = Panel.Panel
            { Panel.panelRenderable = Markdown.renderMarkdown markdownSource
            , Panel.panelTitle = Just "Rendered"
            , Panel.panelBox = rounded
            , Panel.panelStyle = emptyStyle { color = Just (RGB 100 200 100) }
            , Panel.panelExpand = True
            }
    in Columns.columns [rawPanel, renderedPanel]

-- | Create the columns demo with side-by-side panels
makeColumnsDemo :: Columns.Columns
makeColumnsDemo =
    let panel1 = Panel.Panel
            { Panel.panelRenderable = Text.fromMarkup "[bold cyan]Left[/bold cyan]\nContent flows\nvertically"
            , Panel.panelTitle = Just "Panel 1"
            , Panel.panelBox = rounded
            , Panel.panelStyle = emptyStyle { color = Just (RGB 100 200 255) }
            , Panel.panelExpand = True
            }
        panel2 = Panel.Panel
            { Panel.panelRenderable = Text.fromMarkup "[bold yellow]Center[/bold yellow]\nMultiple panels\nside by side"
            , Panel.panelTitle = Just "Panel 2"
            , Panel.panelBox = rounded
            , Panel.panelStyle = emptyStyle { color = Just (RGB 255 200 100) }
            , Panel.panelExpand = True
            }
        panel3 = Panel.Panel
            { Panel.panelRenderable = Text.fromMarkup "[bold green]Right[/bold green]\nAuto-sized\ncolumns"
            , Panel.panelTitle = Just "Panel 3"
            , Panel.panelBox = rounded
            , Panel.panelStyle = emptyStyle { color = Just (RGB 100 255 150) }
            , Panel.panelExpand = True
            }
    in Columns.columns [panel1, panel2, panel3]

-- | Create the text justification demo
makeJustifyDemo :: Columns.Columns
makeJustifyDemo =
    let sampleText = "Word wrap and justify text."
        leftText = Panel.Panel
            { Panel.panelRenderable = Text.leftJustify (Text.fromMarkup sampleText)
            , Panel.panelTitle = Just "left"
            , Panel.panelBox = rounded
            , Panel.panelStyle = emptyStyle { color = Just (RGB 0 200 0) }
            , Panel.panelExpand = True
            }
        centerText = Panel.Panel
            { Panel.panelRenderable = Text.centerJustify (Text.fromMarkup sampleText)
            , Panel.panelTitle = Just "center"
            , Panel.panelBox = rounded
            , Panel.panelStyle = emptyStyle { color = Just (RGB 255 200 0) }
            , Panel.panelExpand = True
            }
        rightText = Panel.Panel
            { Panel.panelRenderable = Text.rightJustify (Text.fromMarkup sampleText)
            , Panel.panelTitle = Just "right"
            , Panel.panelBox = rounded
            , Panel.panelStyle = emptyStyle { color = Just (RGB 100 150 255) }
            , Panel.panelExpand = True
            }
        fullText = Panel.Panel
            { Panel.panelRenderable = Text.fullJustify (Text.fromMarkup sampleText)
            , Panel.panelTitle = Just "full"
            , Panel.panelBox = rounded
            , Panel.panelStyle = emptyStyle { color = Just (RGB 255 100 100) }
            , Panel.panelExpand = True
            }
    in Columns.columns [leftText, centerText, rightText, fullText]
