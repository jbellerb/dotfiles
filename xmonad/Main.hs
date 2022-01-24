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
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
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
import Resources
import Resources.Color

baseConfig = def { modMask = mod1Mask }

dmenuConfig res = [ "-fn", "Roboto Mono:size=14"
                  , "-nb", opaqueColor colorBackground res
                  , "-nf", opaqueColor color15 res
                  , "-sb", opaqueColor color6 res
                  , "-sf", opaqueColor colorBackground res
                  , "-h", show $ dpiScale res 45 ]

myTerminal = "xterm fish"

myKeys modm res =
    -- Terminal configured with myTerminal
    [ ((modm .|. shiftMask, xK_Return)
      , spawn myTerminal)

    -- Lock screen
    , ((modm .|. shiftMask, xK_p)
      , safeSpawn "xlock" [])

    -- Use dmenu to open programs
    , ((modm, xK_p)
      , safeSpawn "dmenu_run" dmenuConfig')

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
    smartBorders $ avoidStruts $
    spacingRaw False (Border b b b b) True (Border b b b b) True $
    tiled ||| Mirror tiled ||| Full
  where
    b = fromIntegral $ dpiScale res 10
    tiled   = ResizableTall nmaster delta ratio []
    nmaster = 1
    delta   = 3/100
    ratio   = 1/2

myManageHook = composeAll
    [ className =? "Xmessage" --> doFloat
    , className =? "pinentry-qt" --> doFloat
    , className =? "Firefox" <&&> stringProperty "WM_NAME" =? "Picture-in-Picture" --> doFloat
    , manageDocks
    ]

lemonbarLogPP res = lemonbarPP
    { ppCurrent = lemonbarColor (opaqueColor color6 res) ""
    , ppSep = " | "
    , ppTitle = shorten 55
    , ppOrder = \(ws:_:t:_) -> [ws, t]
    }

lemonbarConfig res = def
    { barHeight = dpiScale res 45
    , barFonts = ["Roboto Mono:size=14"]
    , barBgColor = opaqueColor colorBackground res
    , barFgColor = opaqueColor colorForeground res
    }

main = do
    res <- currentResources
    mySB <- statusBar (lemonbarConfig res) (pure $ lemonbarLogPP res)
    xmonad $ withSB mySB $ ewmhFullscreen $ docks baseConfig
        { terminal = myTerminal
        , normalBorderColor = opaqueColor colorBackground res
        , focusedBorderColor = opaqueColor color6 res
        , layoutHook = myLayoutHook res
        , manageHook = myManageHook <+> manageHook baseConfig
        } `additionalKeys` myKeys (modMask baseConfig) res
