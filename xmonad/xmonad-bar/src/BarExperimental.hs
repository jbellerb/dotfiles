{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  BarExperimental
Copyright   :  (c) Jared Beller 2022
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable
-}

module BarExperimental (BarConfig(..), barMain) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (asks)
import Graphics.Rendering.Cairo
import Graphics.X11.Xlib.Misc (initThreads)
import System.Exit (exitSuccess)

import Bar.App.Env (Env(..))
import Bar.App.Monad (App, runApp)
import Bar.X11.Draw (clear, MonadDraw, render)
import Bar.X11.Window (closeWindow, GraphicsContext(..), initWindow, Rect(..))

data BarConfig = BarConfig
    { barWidth :: Int
    , barHeight :: Int
    , barX :: Int
    , barY :: Int
    , barFont :: String
    , barName :: String
    , barBgColor :: String
    , barFgColor :: String
    }

loop :: (MonadDraw m, MonadIO m) => Rect -> m ()
loop Rect{..} = do
    -- renderLayout $
    render $ do
        clear
        setSourceRGB 1 1 0
        setLineWidth 10
        setLineCap LineCapRound
        setLineJoin LineJoinRound
        moveTo 10 10
        lineTo (w - 10) 10
        lineTo (w - 10) (h - 10)
        lineTo 10 (h - 10)
        lineTo 10 10
        stroke
    liftIO $ threadDelay (10 * 1000000)
  where
    x = fromIntegral rectX
    y = fromIntegral rectY
    w = fromIntegral rectW
    h = fromIntegral rectH

mkEnv :: BarConfig -> IO Env
mkEnv cfg = Env <$> initWindow x y w h
  where
    x = barX cfg
    y = barY cfg
    w = barWidth cfg
    h = barHeight cfg

cleanEnv :: Env -> IO ()
cleanEnv Env{envContext = ctx} = closeWindow ctx

barMain :: BarConfig -> IO ()
barMain cfg = do
    initThreads
    bracket (mkEnv cfg) cleanEnv (`runApp` (asks (contextGeometry . envContext) >>= loop))
    exitSuccess
