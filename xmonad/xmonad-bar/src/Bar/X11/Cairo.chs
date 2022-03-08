{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module      :  Bar.X11.Cairo
Copyright   :  (c) Jared Beller 2022
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable
-}

module Bar.X11.Cairo
    ( cairoXlibSurfaceCreate
    , cairoCreate
    , cairoSurfaceDestroy
    , cairoDestroy
    ) where

import Graphics.Rendering.Cairo.Types
import Graphics.X11.Types (Drawable)
import Graphics.X11.Xlib.Types (Display(..), Visual(..), Dimension)

#include <X11/Xutil.h>
#include <cairo-xlib.h>

{#pointer *Display newtype nocode #}
{#pointer *Visual newtype nocode #}

{#pointer *cairo_t as Cairo newtype nocode #}
{#pointer *cairo_surface_t as Surface foreign newtype nocode #}

{#fun cairo_xlib_surface_create as ^
    { `Display'
    , fromIntegral `Drawable'
    , `Visual'
    , fromIntegral `Dimension'
    , fromIntegral `Dimension'
    } -> `Surface' mkSurface* #}

{#fun cairo_create as ^ { `Surface' } -> `Cairo' #}

{#fun cairo_surface_destroy as ^ { `Surface' } -> `()' #}

{#fun cairo_destroy as ^ { `Cairo' } -> `()' #}
