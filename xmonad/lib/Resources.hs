{- |
Module      :  Resources
Copyright   :  (c) Jared Beller 2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  non-portable (built for OpenBSD)
-}

module Resources
    ( -- * Resources record
      Resources(..)
    , currentResources

      -- * Resources utilities
    , dpiScale
    , opaqueColor
    , transparentColor
    ) where

import Data.Colour.SRGB (RGB(..), sRGB24show, toSRGB24)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Graphics.X11.Xlib.Display (openDisplay)
import Numeric (showHex)

import Resources.Color
import Resources.XRM

data Resources = Resources
    { resDPI :: Int
    , resColors :: ColorScheme
    } deriving (Show)

currentResources :: IO Resources
currentResources = do
    dpy <- openDisplay ""
    db <- fromJust <$> getServerResources dpy
    Resources
        <$> fmap (maybe 96 read) (queryResources db "Xft.dpi")
        <*> queryScheme db

dpiScale :: Resources -> Int -> Int
dpiScale res n = div (n * resDPI res) 96

opaqueColor :: (ColorScheme -> Color) -> Resources -> String
opaqueColor row = sRGB24show . row . resColors

transparentColor :: (ColorScheme -> Color) -> Word8 -> Resources -> String
transparentColor row a res = ("#" ++) . foldMap showHex2 [a, r, g, b] $ ""
  where
    RGB r g b = toSRGB24 $ row $ resColors res
    showHex2 x
      | x <= 0x0f = ("0" ++) . showHex x
      | otherwise = showHex x
