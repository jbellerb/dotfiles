{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Bar.X11.Draw
Copyright   :  (c) Jared Beller 2022
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable
-}

module Bar.X11.Draw
    ( MonadDraw(..)
    , renderImpl
    , clear
    ) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (asks, MonadReader, runReaderT)
import Graphics.Rendering.Cairo (paint, Operator(..), setOperator, surfaceFlush)
import Graphics.Rendering.Cairo.Internal (Render(Render))
import Graphics.X11.Xlib (clearWindow, flush, setWindowBackgroundPixmap)

import Bar.App.Env (Env(envContext))
import Bar.App.Monad (App)
import Bar.X11.Window (GraphicsContext(..))

class Monad m => MonadDraw m where
    render :: Render a -> m a

instance MonadDraw App where
    render = renderImpl

renderImpl :: (MonadIO m, MonadReader Env m) => Render a -> m a
renderImpl (Render m) = do
    GraphicsContext{..} <- asks envContext
    liftIO $ do
        r <- runReaderT m contextCairo
        surfaceFlush contextSurface
        setWindowBackgroundPixmap contextDisplay contextWindow contextPixmap
        clearWindow contextDisplay contextWindow
        flush contextDisplay
        return r

clear :: Render ()
clear = do
    setOperator OperatorClear
    paint
    setOperator OperatorOver
