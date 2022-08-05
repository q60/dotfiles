{-# LANGUAGE OverloadedStrings #-}

module Main where

import Termonad.App (defaultMain)
import Termonad.Config
  ( FontConfig, FontSize ( FontSizePoints ), Option ( Set )
  , ShowScrollbar ( ShowScrollbarIfNeeded ), defaultConfigOptions, defaultFontConfig
  , defaultTMConfig, fontConfig, fontFamily, fontSize, options, showScrollbar
  , showMenu
  )
import Termonad.Config.Colour
  ( AlphaColour, ColourConfig, addColourExtension, createColour
  , createColourExtension, cursorFgColour, cursorBgColour
  , defaultColourConfig, Palette ( ExtendedPalette )
  , palette, List8, unsafeMkList8
  , foregroundColour, backgroundColour
  )
import Numeric ( readHex )


-- | Converts a hex string to colour.
makeColour :: [Char] -> AlphaColour Double
makeColour str = let uncurry3 f [r,g,b] = f r g b
                 in uncurry3 createColour (colourFromStr str)
                    where colourFromStr (x:xs) = let rgb = [ take 2 xs
                                                           , take 2 $ drop 2 xs
                                                           , take 2 $ drop 4 xs
                                                           ]
                                                     dec = fst . head . readHex
                                                 in dec <$> rgb


-- | Converts a list of hex strings to the List8 of colours.
makeColourTable :: [[Char]] -> List8 (AlphaColour Double)
makeColourTable xs = unsafeMkList8 $ makeColour <$> xs


-- | This sets the foreground colour of the cursor in the terminal.
cursFgColour :: AlphaColour Double
cursFgColour = makeColour "#0D1017"


-- | This sets the background colour of the cursor in the terminal.
cursBgColour :: AlphaColour Double
cursBgColour = makeColour "#F8F9FA"


-- | This sets the foreground colour of the terminal.
termFgColour :: AlphaColour Double
termFgColour = cursBgColour


-- | This sets the background colour of the terminal.
termBgColour :: AlphaColour Double
termBgColour = cursFgColour


-- | This sets normal ANSI colours.
paletteNormal :: List8 (AlphaColour Double)
paletteNormal =
  makeColourTable [ "#122132" -- black
                  , "#e65050" -- red
                  , "#6cbf43" -- green
                  , "#f2ae49" -- yellow
                  , "#399ee6" -- blue
                  , "#a37acc" -- magenta
                  , "#55b4d4" -- cyan
                  , "#EAE7EB" -- white
                  ]


-- | This sets bright ANSI colours.
paletteBright :: List8 (AlphaColour Double)
paletteBright =
  makeColourTable [ "#23344B" -- black
                  , "#ff6666" -- red
                  , "#87d96c" -- green
                  , "#ffd173" -- yellow
                  , "#73d0ff" -- blue
                  , "#dfbfff" -- magenta
                  , "#5ccfe6" -- cyan
                  , "#F8F9FA" -- white
                  ]


-- | This sets the colours used for the terminal.
colConf :: ColourConfig (AlphaColour Double)
colConf =
  defaultColourConfig
    { cursorBgColour = Set cursBgColour
    , cursorFgColour = Set cursFgColour
    , backgroundColour = Set termBgColour
    , foregroundColour = Set termFgColour
    , palette = ExtendedPalette paletteNormal paletteBright
    }

-- | This defines the font for the terminal.
fontConf :: FontConfig
fontConf =
  defaultFontConfig
    { fontFamily = "Iosevka Term Slab"
    , fontSize = FontSizePoints 13
    }

main :: IO ()
main = do
  colExt <- createColourExtension colConf
  let termonadConf =
        defaultTMConfig
          { options =
              defaultConfigOptions
                { fontConfig    = fontConf
                , showScrollbar = ShowScrollbarIfNeeded
                , showMenu      = False
                }
          }
        `addColourExtension` colExt
  defaultMain termonadConf
