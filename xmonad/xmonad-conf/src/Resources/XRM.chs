{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Resources.XRM
Copyright   :  (c) Jared Beller 2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  non-portable (requires OpenBSD)
-}

module Resources.XRM
    ( -- * Xresources database
      XrmDatabase
    , getServerResources

      -- * Database access
    , queryResources
    ) where

import Foreign (alloca, peek, Storable)
import Foreign.C.String (CString, peekCString)
import Foreign.Ptr (castPtr, nullPtr)
import Graphics.X11.Xlib (Display(..))

#include <X11/Xresource.h>

{#pointer *Display newtype nocode #}

{#pointer XrmDatabase newtype #}
    deriving (Eq, Show)

data XrmValue = XrmValue
    { xrmValueSize :: Word
    , xrmValueAddr :: CString
    } deriving (Show)

instance Storable XrmValue where
    sizeOf _ = {#sizeof XrmValue #}
    alignment _ = {#alignof XrmValue #}

    peek p = do
        size <- {#get XrmValue->size #} p
        addr <- {#get XrmValue->addr #} p
        return $ XrmValue (fromIntegral size) addr

    poke p XrmValue{..} = do
        {#set XrmValue.size #} p $ fromIntegral xrmValueSize
        {#set XrmValue.addr #} p xrmValueAddr

getServerResources :: Display -> IO (Maybe XrmDatabase)
getServerResources dpy = do
    s <- xResourceManagerString dpy
    if s == nullPtr
        then return Nothing
        else do
          db <- xrmGetStringDatabase s
          return $ if db == XrmDatabase nullPtr
              then Nothing
              else Just db

{# fun XResourceManagerString as ^ { `Display' } -> `CString' #}
{# fun XrmGetStringDatabase as ^ { `CString' } -> `XrmDatabase' #}

queryResources :: XrmDatabase -> String -> IO (Maybe String)
queryResources db key = do
    (resource, _, XrmValue{..}) <- xrmGetResource db key key
    if resource
        then Just <$> peekCString xrmValueAddr
        else return Nothing

{# fun XrmGetResource as ^
    { `XrmDatabase'
    , `String'
    , `String'
    , alloca- `CString' peek*
    , alloca- `XrmValue' 'peek . castPtr'*
    } -> `Bool' #}
