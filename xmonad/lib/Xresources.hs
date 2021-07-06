{- |
Module      :  Xresources
Copyright   :  (c) Jared Beller 2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  non-portable (requires OpenBSD)
-}

module Xresources
    ( -- * Xresources record
      Xresources(..)
    , currentResources

      -- * Xresources utilities
    , dpiScale
    ) where

import Data.Maybe (fromJust)
import Graphics.X11.Xlib.Display (openDisplay)

import Xresources.XRM

data Xresources = Xresources
    { xrdbDPI :: Int
    } deriving (Show)

currentResources :: IO Xresources
currentResources = do
    dpy <- openDisplay ""
    db <- fromJust <$> getServerResources dpy
    dpi <- queryResources db "Xft.dpi"
    return $ Xresources
        { xrdbDPI = maybe 96 read dpi
        }

dpiScale :: Xresources -> Int -> Int
dpiScale resources n = div (n * xrdbDPI resources) 96
