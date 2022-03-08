{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  BarExperimental
Copyright   :  (c) Jared Beller 2022
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable
-}

module BarExperimental (barMain) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks, runReaderT)
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Internal (Render(..))
import Graphics.Rendering.Cairo.Types (Cairo)
import Graphics.X11.Xlib (clearWindow, flush, setWindowBackgroundPixmap)
import Graphics.X11.Xlib.Misc (initThreads)
import System.Exit (exitWith, ExitCode(..))

import Bar.App.Env
import Bar.App.Monad
import Bar.X11.Window (closeWindow, GraphicsContext(..), initWindow)

loop :: App ()
loop = do
    drawCairo $ do
        setOperator OperatorClear
        paint
        setOperator OperatorAdd
        setSourceRGB 1 1 0
        setLineWidth 10
        setLineCap LineCapRound
        setLineJoin LineJoinRound
        moveTo 30 30
        lineTo 70 30
        lineTo 70 70
        lineTo 30 70
        lineTo 30 30
        stroke
    liftIO $ threadDelay (10 * 1000000)
  where
    drawCairo :: Render () -> App ()
    drawCairo (Render m) = do
        GraphicsContext{..} <- asks envContext
        liftIO $ do
            runReaderT m contextCairo
            surfaceFlush contextSurface
            setWindowBackgroundPixmap contextDisplay contextWindow contextPixmap
            clearWindow contextDisplay contextWindow
            flush contextDisplay

mkEnv :: IO Env
mkEnv = Env <$> initWindow

cleanEnv :: Env -> IO ()
cleanEnv Env{envContext = ctx} = closeWindow ctx

barMain :: IO ()
barMain = do
    initThreads
    bracket mkEnv cleanEnv (\env -> runApp env loop)
    exitWith ExitSuccess
