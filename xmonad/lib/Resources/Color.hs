{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Resources.Color
Copyright   :  (c) Jared Beller 2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  non-portable (built for OpenBSD)
-}

module Resources.Color
    ( Color
    , ColorScheme(..)
    , queryScheme
    ) where

import Data.Colour (Colour)
import Data.Colour.SRGB (sRGB24read)
import Data.Default (def, Default)

import Resources.XRM

type Color = Colour Double

data ColorScheme = ColorScheme
    { colorBackground :: Color
    , colorForeground :: Color
    , color0 :: Color
    , color1 :: Color
    , color2 :: Color
    , color3 :: Color
    , color4 :: Color
    , color5 :: Color
    , color6 :: Color
    , color7 :: Color
    , color8 :: Color
    , color9 :: Color
    , color10 :: Color
    , color11 :: Color
    , color12 :: Color
    , color13 :: Color
    , color14 :: Color
    , color15 :: Color
    } deriving (Show)

instance Default ColorScheme where
    def = ColorScheme
        { colorBackground = sRGB24read "#000000"
        , colorForeground = sRGB24read "#FFFFFF"
        , color0 = sRGB24read "#000000"
        , color1 = sRGB24read "#AA0000"
        , color2 = sRGB24read "#00AA00"
        , color3 = sRGB24read "#AA5500"
        , color4 = sRGB24read "#0000AA"
        , color5 = sRGB24read "#AA00AA"
        , color6 = sRGB24read "#00AAAA"
        , color7 = sRGB24read "#AAAAAA"
        , color8 = sRGB24read "#555555"
        , color9 = sRGB24read "#FF5555"
        , color10 = sRGB24read "#55FF55"
        , color11 = sRGB24read "#FFFF55"
        , color12 = sRGB24read "#5555FF"
        , color13 = sRGB24read "#FF55FF"
        , color14 = sRGB24read "#55FFFF"
        , color15 = sRGB24read "#FFFFFF"
        }

queryScheme :: XrmDatabase -> IO ColorScheme
queryScheme db = ColorScheme
    <$> getColor "background" colorBackground
    <*> getColor "foreground" colorForeground
    <*> getColor "color0" color0
    <*> getColor "color1" color1
    <*> getColor "color2" color2
    <*> getColor "color3" color3
    <*> getColor "color4" color4
    <*> getColor "color5" color5
    <*> getColor "color6" color6
    <*> getColor "color7" color7
    <*> getColor "color8" color8
    <*> getColor "color9" color9
    <*> getColor "color10" color10
    <*> getColor "color11" color11
    <*> getColor "color12" color12
    <*> getColor "color13" color13
    <*> getColor "color14" color14
    <*> getColor "color15" color15
  where
    getColor field row =
        parseColor row <$> queryResources db ("Xmonad." ++ field)
    parseColor row = maybe (row def) sRGB24read
