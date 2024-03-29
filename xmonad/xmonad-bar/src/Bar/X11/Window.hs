{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Bar.X11.Window
Copyright   :  (c) Jared Beller 2022
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable
-}

module Bar.X11.Window
    ( Rect(..)
    , GraphicsContext(..)
    , initWindow
    , closeWindow
    ) where

import Data.Bits ((.|.))
import Data.Maybe (fromJust)
import Graphics.Rendering.Cairo.Types (Cairo, Surface)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import Bar.X11.Cairo

data Rect = Rect
    { rectX :: Position
    , rectY :: Position
    , rectW :: Dimension
    , rectH :: Dimension
    }

data GraphicsContext = GraphicsContext
    { contextDisplay :: Display
    , contextWindow :: Window
    , contextPixmap :: Drawable
    , contextGeometry :: Rect
    , contextSurface :: Surface
    , contextCairo :: Cairo
    }

initWindow :: Int -> Int -> Int -> Int -> IO GraphicsContext
initWindow x y w h = do
    dpy <- openDisplay ""
    let geo = resolveGeometry dpy x y w h
    (win, vis) <- newWindow dpy geo
    setWindowProperties dpy win
    (pix, sfc, ctx) <- initCairo dpy win vis
    clearWindow dpy win
    mapWindow dpy win
    flush dpy
    return $ GraphicsContext dpy win pix geo sfc ctx

resolveGeometry :: Display -> Int -> Int -> Int -> Int -> Rect
resolveGeometry dpy x y w h = Rect
    { rectX = fromIntegral x
    , rectY = fromIntegral y
    , rectW = if w < 0 then widthOfScreen scr else fromIntegral w
    , rectH = if h < 0 then heightOfScreen scr else fromIntegral h
    }
  where
    scr = defaultScreenOfDisplay dpy

newWindow :: Display -> Rect -> IO (Window, VisualInfo)
newWindow dpy Rect{..} = do
    visual@VisualInfo{..} <- getVisual dpy
    root <- rootWindow dpy (defaultScreen dpy)
    colormap <- createColormap dpy root visualInfo_visual allocNone
    win <- allocaSetWindowAttributes $
        \attributes -> do
            let eventMask = structureNotifyMask .|. buttonPressMask
                attrMask = cWBackPixel .|. cWBorderPixel .|. cWColormap
                    .|. cWEventMask
            set_background_pixel attributes 0
            set_border_pixel attributes 0
            set_colormap attributes colormap
            set_event_mask attributes eventMask
            createWindow dpy root rectX rectY rectW rectH 0 visualInfo_depth
                inputOutput visualInfo_visual attrMask attributes
    return (win, visual)

getVisual :: Display -> IO VisualInfo
getVisual dpy = do
    visual <- matchVisualInfo dpy (defaultScreen dpy) 32 trueColor
    return $ fromJust visual
  where
    trueColor = 4

setWindowProperties :: Display -> Window -> IO ()
setWindowProperties dpy win = do
    wmWindowType <- internAtom dpy "_NET_WM_WINDOW_TYPE" False
    wmWindowTypeDock <- internAtom dpy "_NET_WM_WINDOW_TYPE_DOCK" False
    changeProperty32 dpy win wmWindowType aTOM propModeReplace
        [fromIntegral wmWindowTypeDock]

    wmState <- internAtom dpy "_NET_WM_STATE" False
    wmStateSticky <- internAtom dpy "_NET_WM_STATE_STICKY" False
    wmStateAbove <- internAtom dpy "_NET_WM_STATE_ABOVE" False
    changeProperty32 dpy win wmState aTOM propModeReplace $
        map fromIntegral [wmStateSticky, wmStateAbove]

    storeName dpy win "bar"
    setClassHint dpy win $ ClassHint "xmonad-bar" "bar"

initCairo :: Display -> Window -> VisualInfo -> IO (Pixmap, Surface, Cairo)
initCairo dpy win VisualInfo{..} = do
    (_, x, y, w, h, _, _) <- getGeometry dpy win
    pixmap <- createPixmap dpy win w h visualInfo_depth
    surface <- cairoXlibSurfaceCreate dpy pixmap visualInfo_visual w h
    cairo <- cairoCreate surface
    return (pixmap, surface, cairo)

closeWindow :: GraphicsContext -> IO ()
closeWindow GraphicsContext{..} = do
    freePixmap contextDisplay contextPixmap
    cairoSurfaceDestroy contextSurface
    cairoDestroy contextCairo
    destroyWindow contextDisplay contextWindow
    closeDisplay contextDisplay
