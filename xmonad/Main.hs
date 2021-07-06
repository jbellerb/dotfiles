{- |
Module      :  Main
Copyright   :  (c) Jared Beller 2020-2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  non-portable (built for OpenBSD)
-}

module Main where

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Util.Dmenu (menuArgs)
import XMonad.Util.Run (safeSpawn, spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)

import Control.Monad (when)
import Data.Default (def)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.IO

import Bar
import Lemonbar
import Xresources
import qualified Colors

baseConfig = fullscreenSupport $ desktopConfig { modMask = mod1Mask }

dmenuConfig res = [ "-fn", "Roboto Mono:size=14"
                  , "-nb", Colors.background
                  , "-nf", Colors.color15
                  , "-sb", Colors.color6
                  , "-sf", Colors.background
                  , "-h", show $ dpiScale res 45 ]

myTerminal = "xterm fish"

myKeys modm res =
    -- Terminal configured with myTerminal
    [ ((modm .|. shiftMask, xK_Return)
      , spawn myTerminal)

    -- Lock screen
    , ((modm .|. shiftMask, xK_p)
      , spawn "xlock")

    -- Use dmenu to open programs
    , ((modm, xK_p)
      , safeSpawn "dmenu_run" $ dmenuConfig')

    -- Exit confirmation
    , ((modm .|. shiftMask, xK_q)
      , menuArgs "dmenu" ("-i" : dmenuConfig') [ "Cancel", "Logout" ]
        >>= (\status -> when (status == "Logout") $ io exitSuccess))

    -- Shrink and expand secondary panes
    , ((modm .|. shiftMask, xK_h), sendMessage MirrorShrink)
    , ((modm .|. shiftMask, xK_l), sendMessage MirrorExpand)
    -- -- Use yeganesh to open programs
    -- , ((modm, xK_p)
    --   , spawn "yeganesh -x")
    ]
  where
    dmenuConfig' = dmenuConfig res

myLayoutHook res =
    avoidStruts $
    spacingRaw False (Border b b b b) True (Border b b b b) True $
    tiled ||| Mirror tiled ||| Full
  where
    b = fromIntegral $ dpiScale res 10
    tiled   = ResizableTall nmaster delta ratio []
    nmaster = 1
    delta   = 3/100
    ratio   = 1/2

lemonbarLogPP barproc = lemonbarPP
    { ppOutput = hPutStrLn barproc
    , ppCurrent = lemonbarColor Colors.color6 ""
    , ppSep = " | "
    , ppTitle = shorten 55
    , ppOrder = \(ws:_:t:_) -> [ws, t]
    }

lemonbarConfig res = def
    { barHeight = dpiScale res 45
    , barFonts = ["Roboto Mono:size=14"]
    , barBgColor = "#2E3440"
    , barFgColor = "#D8DEE9"
    }

main = do
    resources <- currentResources
    barproc <- spawnBar $ lemonbarConfig resources
    xmonad $ baseConfig
        { terminal = myTerminal
        , normalBorderColor = Colors.background
        , focusedBorderColor = Colors.color6
        , layoutHook = myLayoutHook resources
        , manageHook = manageDocks <+> manageHook baseConfig
        , handleEventHook = handleEventHook baseConfig <+> docksEventHook
        , logHook = dynamicLogWithPP (lemonbarLogPP barproc) >> logHook baseConfig
        } `additionalKeys` myKeys (modMask baseConfig) resources
