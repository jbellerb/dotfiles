module Main where

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
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
import Bar.Lemonbar
import qualified Colors

baseConfig = desktopConfig { modMask = mod1Mask }

dmenuConfig = [ "-fn", "Roboto Mono:size=14"
              , "-nb", Colors.background
              , "-nf", Colors.color15
              , "-sb", Colors.color6
              , "-sf", Colors.background
              , "-h", "60" ]

myTerminal = "xterm fish"

myKeys modm =
    -- Terminal configured with myTerminal
    [ ((modm .|. shiftMask, xK_Return)
      , spawn myTerminal)

    -- Lock screen
    , ((modm .|. shiftMask, xK_p)
      , spawn "xlock")

    -- Use dmenu to open programs
    , ((modm, xK_p)
      , safeSpawn "dmenu_run" dmenuConfig)

    -- Exit confirmation
    , ((modm .|. shiftMask, xK_q)
      , menuArgs "dmenu" ("-i" : dmenuConfig) [ "Cancel", "Logout" ]
        >>= (\status -> when (status == "Logout") $ io exitSuccess))

    -- Shrink and expand secondary panes
    , ((modm .|. shiftMask, xK_h), sendMessage MirrorShrink)
    , ((modm .|. shiftMask, xK_l), sendMessage MirrorExpand)
    -- -- Use yeganesh to open programs
    -- , ((modm, xK_p)
    --   , spawn "yeganesh -x")
    ]

myLayoutHook =
    avoidStruts $
    spacingRaw False (Border 15 15 15 15) True (Border 15 15 15 15) True $
    tiled ||| Mirror tiled ||| Full
  where
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

lemonbarConfig = def
    { barHeight = 60
    , barFonts = ["Roboto Mono:size=14"]
    , barBgColor = "#2E3440"
    , barFgColor = "#D8DEE9"
    }

main = do
    barproc <- spawnBar lemonbarConfig
    xmonad $ baseConfig
        { terminal = myTerminal
        , normalBorderColor = Colors.background
        , focusedBorderColor = Colors.color6
        , layoutHook = myLayoutHook
        , manageHook = manageDocks <+> manageHook baseConfig
        , handleEventHook = handleEventHook baseConfig <+> docksEventHook
        , logHook = dynamicLogWithPP (lemonbarLogPP barproc) >> logHook baseConfig
        } `additionalKeys` myKeys (modMask baseConfig)
