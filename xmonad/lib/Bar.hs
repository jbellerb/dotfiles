{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Bar
Copyright   :  (c) Jared Beller 2020-2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  non-portable (built for OpenBSD)
-}

module Bar (spawnBar) where

import Control.Concurrent (newEmptyMVar, MVar, putMVar, takeMVar)
import Control.Concurrent.Async (Concurrently(..), runConcurrently)
import Control.Concurrent.STM (atomically, newTVar, readTVarIO, TVar, writeTVar)
import Control.Monad (forever, void)
import Data.Foldable (traverse_)
import System.IO (BufferMode(..), Handle, hPutStrLn, hSetBuffering)
import System.Posix.IO
import XMonad (xfork)
import XMonad.Util.Run (spawnPipe)

import Bar.Modules
import Lemonbar

data Bar = Bar
    { barOutput :: !(String ->IO ())
    , barWaker :: !(MVar ())
    , barXmonadInfo :: !(TVar String)
    , barTime :: !(TVar String)
    , barBattery :: !(TVar String)
    , barUname :: !(TVar String)
    }

layoutSections :: String -> String -> String -> String
layoutSections left center right =
    concat ["%{l}", left, "%{c}", center, "%{r}", right] 

barRefresh :: Bar -> IO ()
barRefresh bar@Bar{..} = do
    info <- readTVarIO barXmonadInfo
    time <- readTVarIO barTime
    battery <- readTVarIO barBattery
    uname <- readTVarIO barUname
    let padding = "  "
        left = concat [padding, info]
        center = time
        right = concat [battery, " | ", uname, padding]
    barOutput $ layoutSections left center right

runBar :: LemonbarConfig -> IO ()
runBar lemonbarConfig = do
    lbproc <- spawnPipe $ "lemonbar-xft" ++ renderConfig lemonbarConfig
    barWaker <- newEmptyMVar
    barXmonadInfo <- atomically $ newTVar ""
    barTime <- atomically $ newTVar ""
    barBattery <- atomically $ newTVar ""
    barUname <- atomically $ newTVar ""
    let bar = Bar { barOutput = hPutStrLn lbproc, .. }
        waker = putMVar barWaker ()
        modules =
            [ (refreshXmonadInfo, barXmonadInfo)
            , (refreshTime, barTime)
            , (refreshBattery, barBattery)
            , (refreshUname, barUname)
            ]
    runConcurrently $ void $ (,)
        <$> traverse_ (spawnModule waker) modules
        <*> Concurrently (listener barWaker bar)
  where
    spawnModule waker (mod, info) = Concurrently $ mod info waker
    listener barWaker bar = forever $ do
        _ <- takeMVar barWaker
        barRefresh bar

spawnBar :: LemonbarConfig -> IO Handle
spawnBar lemonbarConfig = do
    (rd, wr) <- createPipe
    setFdOption wr CloseOnExec True
    h <- fdToHandle wr
    hSetBuffering h LineBuffering
    _ <- xfork $ do
          _ <- dupTo rd stdInput
          runBar lemonbarConfig
    closeFd rd
    return h
