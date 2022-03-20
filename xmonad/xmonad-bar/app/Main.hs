{- |
Module      :  Main
Copyright   :  (c) Jared Beller 2022
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable
-}

module Main (main) where

import BarExperimental (BarConfig(..), barMain)

bar :: BarConfig
bar = BarConfig
    { barWidth = -1
    , barHeight = 45
    , barX = 0
    , barY = 0
    , barFont = "Roboto Mono:size=14"
    , barName = "xmonad-bar"
    , barBgColor = ""
    , barFgColor = ""
    }

main :: IO ()
main = barMain bar
