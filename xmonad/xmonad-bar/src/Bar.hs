{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Bar
Copyright   :  (c) Jared Beller 2020-2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  non-portable (built for OpenBSD)
-}

module Bar (statusBar) where

import Control.Concurrent (newEmptyMVar, MVar, putMVar, takeMVar)
import Control.Concurrent.Async (Concurrently(..), runConcurrently)
import Control.Concurrent.STM (atomically, newTVar, readTVarIO, TVar, writeTVar)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Environment (getArgs, getExecutablePath)
import System.IO
import System.Posix.IO
import System.Posix.Process (executeFile)
import XMonad (X, xfork)
import XMonad.Hooks.StatusBar (StatusBarConfig(..))
import XMonad.Hooks.StatusBar.PP (dynamicLogString, PP)

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
barRefresh Bar{..} = do
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
    lbproc <- spawnFilePipe "lemonbar-xft" $ renderConfig lemonbarConfig
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

spawnFilePipe :: FilePath -> [String] -> IO Handle
spawnFilePipe path args = do
    (rd, wr) <- createPipe
    setFdOption wr CloseOnExec True
    h <- fdToHandle wr
    hSetEncoding h localeEncoding
    hSetBuffering h LineBuffering
    void $ xfork $ do
        void $ dupTo rd stdInput
        executeFile path True args Nothing
    closeFd rd
    return h

statusBar :: LemonbarConfig -> X PP -> IO StatusBarConfig
statusBar conf xpp = do
    args <- getArgs
    case args of
        ["-B"] -> runBar conf
        _ -> return ()
    hRef <- newIORef Nothing
    return $ StatusBarConfig
        { sbStartupHook = liftIO $ do
            h <- flip spawnFilePipe ["-B"] =<< getExecutablePath
            writeIORef hRef $ Just h
        , sbLogHook = do
            pp <- xpp
            msg <- dynamicLogString pp
            liftIO $ do
                h' <- readIORef hRef
                maybe (return ()) (`hPutStrLn` msg) h'
        , sbCleanupHook = liftIO $ do
            h' <- readIORef hRef
            maybe (return ()) (\h -> hClose h >> writeIORef hRef Nothing) h'
        }
