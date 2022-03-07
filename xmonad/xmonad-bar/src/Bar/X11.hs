{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Bar.X11
Copyright   :  (c) Jared Beller 2022
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable
-}

module Bar.X11 (initWindow) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Bits ((.|.))
import Data.ByteString (unpack)
import Data.ByteString.Char8 (pack)
import Data.Int (Int32)
import Data.Word (Word32)
import Foreign.C.String (withCString)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

initWindow :: IO (Display, Maybe Window)
initWindow = do
    d <- openDisplay ""
    w <- runMaybeT $ do
        w <- MaybeT $ newWindow d (0, 0, 100, 100)
        lift $ do
            setWindowProperties d w
            mapWindow d w
            sync d False
            return w
    return (d, w)

newWindow :: Display -> (Int32, Int32, Word32, Word32) -> IO (Maybe Window)
newWindow d (x, y, w, h) = runMaybeT $ do
    let screen = defaultScreen d
    VisualInfo{..} <- MaybeT $ matchVisualInfo d screen 32 trueColor
    root <- lift $ rootWindow d screen
    colormap <- lift $ createColormap d root visualInfo_visual allocNone
    lift $ allocaSetWindowAttributes $
        \attributes -> do
            let eventMask = structureNotifyMask .|. buttonPressMask
                attrMask = cWBackPixel .|. cWBorderPixel .|. cWColormap
                    .|. cWEventMask
            set_background_pixel attributes $ whitePixel d screen
            set_border_pixel attributes 0
            set_colormap attributes colormap
            set_event_mask attributes eventMask
            createWindow d root x y w h 0 visualInfo_depth inputOutput
                visualInfo_visual attrMask attributes
  where
    trueColor = 4

setWindowProperties :: Display -> Window -> IO ()
setWindowProperties d w = do
    wmWindowType <- internAtom d "_NET_WM_WINDOW_TYPE" False
    wmWindowTypeDock <- internAtom d "_NET_WM_WINDOW_TYPE_DOCK" False
    changeProperty32
        d w wmWindowType aTOM propModeReplace [fromIntegral wmWindowTypeDock]

    wmState <- internAtom d "_NET_WM_STATE" False
    wmStateSticky <- internAtom d "_NET_WM_STATE_STICKY" False
    wmStateAbove <- internAtom d "_NET_WM_STATE_ABOVE" False
    changeProperty32 d w wmState aTOM propModeReplace $
        map fromIntegral [wmStateSticky, wmStateAbove]

    storeName d w "bar"
    setClassHint d w $ ClassHint "xmonad-bar" "bar"
