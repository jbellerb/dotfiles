{- |
Module      :  Bar.App.Env
Copyright   :  (c) Jared Beller 2022
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable
-}

module Bar.App.Env
    ( Env(..)
    ) where

import Bar.X11.Window (GraphicsContext)

data Env = Env
    { envContext :: !GraphicsContext
    }
