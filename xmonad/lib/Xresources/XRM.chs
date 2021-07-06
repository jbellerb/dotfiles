{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Xresources.XRM
Copyright   :  (c) Jared Beller 2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  non-portable (requires OpenBSD)
-}

module Xresources.XRM
    ( -- * Xresources database
      XrmDatabase
    , getServerResources

      -- * Database access
    , queryResources
    ) where

import Foreign (alloca, peek, Storable)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.Ptr (IntPtr, intPtrToPtr, Ptr, ptrToIntPtr, nullPtr)
import Graphics.X11.Xlib (Display(..))

#include <X11/Xresource.h>

{#pointer *XrmDatabase newtype #}
    deriving (Eq, Show)

data XrmValue = XrmValue
    { xrmValueSize :: Word
    , xrmValueAddr :: IntPtr
    } deriving (Show)

instance Storable XrmValue where
    sizeOf _ = {#sizeof XrmValue #}
    alignment _ = {#alignof XrmValue #}

    peek p = do
        size <- {#get XrmValue->size #} p
        addr <- {#get XrmValue->addr #} p
        return $ XrmValue
            (fromIntegral size)
            (ptrToIntPtr addr)
 
    poke p XrmValue{..} = do
        {#set XrmValue.size #} p $ fromIntegral xrmValueSize
        {#set XrmValue.addr #} p $ intPtrToPtr xrmValueAddr

getServerResources :: Display -> IO (Maybe XrmDatabase)
getServerResources dpy = makeDatabase =<< xResourceManagerString dpy
  where
    makeDatabase s
      | s == nullPtr = return Nothing
      | otherwise = do
          db <- xrmGetStringDatabase s
          return $ if db == XrmDatabase nullPtr
              then Nothing
              else Just db
foreign import ccall unsafe "XResourceManagerString"
    xResourceManagerString :: Display -> IO CString
foreign import ccall unsafe "XrmGetStringDatabase"
    xrmGetStringDatabase :: CString -> IO XrmDatabase

queryResources :: XrmDatabase -> String -> IO (Maybe String)
queryResources db key =
    withCString key $ \key' ->
    alloca $ \rType' ->
    alloca $ \rValue' -> do
        resource <- xrmGetResource db key' key' rType' rValue'
        if resource
            then do
                rValue <- peek rValue'
                Just <$> (peekCString . intPtrToPtr . xrmValueAddr) rValue
            else return Nothing
foreign import ccall unsafe "XrmGetResource"
    xrmGetResource ::
        XrmDatabase -> CString -> CString -> Ptr CString -> Ptr XrmValue -> IO Bool
