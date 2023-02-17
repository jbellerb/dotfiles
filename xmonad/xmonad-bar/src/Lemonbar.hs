{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Lemonbar
Copyright   :  (c) Jared Beller 2020
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable
-}

module Lemonbar
    ( -- * Configuration args
      LemonbarConfig(..)
    , renderConfig

      -- * Pretty print formatter
    , lemonbarColor
    , lemonbarEscape
    , lemonbarPP
    ) where

import Data.Default (def, Default)
import System.IO (Handle)
import XMonad.Hooks.StatusBar.PP (PP, ppTitleSanitize, wrap)
import XMonad.Util.Run (spawnPipe)

data LemonbarConfig = LemonbarConfig
    { barWidth :: Int
    , barHeight :: Int
    , barX :: Int
    , barY :: Int
    , barBottom :: Bool
    , barForceDocking :: Bool
    , barFonts :: [String]
    , barClickables :: Int
    , barName :: String
    , barUnderlineWidth :: Int
    , barBgColor :: String
    , barFgColor :: String
    , barUlColor :: String
    }

instance Default LemonbarConfig where
    def = LemonbarConfig
        { barWidth = -1
        , barHeight = -1
        , barX = -1
        , barY = -1
        , barBottom = False
        , barForceDocking = False
        , barFonts = []
        , barClickables = -1
        , barName = ""
        , barUnderlineWidth = -1
        , barBgColor = ""
        , barFgColor = ""
        , barUlColor = ""
        }

renderConfig :: LemonbarConfig -> [String]
renderConfig LemonbarConfig{..} = concat
    [ ["-g", geometry]
    , ["-b" | barBottom]
    , ["-d" | barForceDocking]
    , concatMap (\font -> ["-f", font]) barFonts
    , if barClickables == -1 then [] else ["-a", show barClickables]
    , if null barName then [] else ["-n", barName]
    , if barUnderlineWidth == -1 then [] else ["-u", show barUnderlineWidth]
    , if null barBgColor then [] else ["-B", barBgColor]
    , if null barFgColor then [] else ["-F", barFgColor]
    , if null barUlColor then [] else ["-U", barUlColor]
    ]
  where
    geometry =
        concat [val barWidth, "x", val barHeight, "+", val barX, "+", val barY]
    val n
        | n > 0 = show n
        | otherwise = ""

lemonbarColor :: String -> String -> String -> String
lemonbarColor fg bg = wrap (fgs ++ bgs) (fge ++ bge)
  where
    (fgs, fge)
      | null fg = ("", "")
      | otherwise = (concat ["%{F", fg, "}"], "%{F-}")
    (bgs, bge)
      | null bg = ("", "")
      | otherwise = (concat ["%{B", bg, "}"], "%{B-}")

lemonbarEscape :: String -> String
lemonbarEscape = concatMap (\x -> if x == '%' then "%%" else [x])

lemonbarPP :: PP
lemonbarPP = def { ppTitleSanitize = lemonbarEscape }
