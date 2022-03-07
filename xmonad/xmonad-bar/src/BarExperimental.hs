{- |
Module      :  Bar
Copyright   :  (c) Jared Beller 2022
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable
-}

module BarExperimental (barMain) where

import Control.Concurrent (threadDelay)
import Graphics.X11.Xlib.Misc (initThreads)
import System.Exit (exitWith, ExitCode(..))

import Bar.X11 (initWindow)

barMain :: IO ()
barMain = do
    initThreads
    (d, w) <- initWindow
    threadDelay (10 * 1000000)
    exitWith ExitSuccess
