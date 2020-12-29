{-# LANGUAGE RecordWildCards #-}

module Bar.Lemonbar
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
import XMonad (MonadIO)
import XMonad.Hooks.DynamicLog (PP, ppTitleSanitize, wrap)
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

renderConfig :: LemonbarConfig -> String
renderConfig LemonbarConfig{..} = concat
    [ " -g " ++ geometry
    , if barBottom then " -b" else ""
    , if barForceDocking then " -d" else ""
    , concatMap (\x -> " -f " ++ show x) barFonts
    , if barClickables == -1 then "" else " -a " ++ show barClickables
    , if null barName then "" else " -n " ++ show barName
    , if barUnderlineWidth == -1 then "" else " -u " ++ show barUnderlineWidth
    , if null barBgColor then "" else " -B " ++ show barBgColor
    , if null barFgColor then "" else " -F " ++ show barFgColor
    , if null barUlColor then "" else " -U " ++ show barUlColor
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
