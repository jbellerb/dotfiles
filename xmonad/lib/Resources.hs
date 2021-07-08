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
    , colorString
    ) where

import Data.Colour.SRGB (sRGB24show)
import Data.Maybe (fromJust)
import Graphics.X11.Xlib.Display (openDisplay)

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

colorString :: (ColorScheme -> Color) -> Resources -> String
colorString row = sRGB24show . row . resColors
