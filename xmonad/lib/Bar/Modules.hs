{-# LANGUAGE RecordWildCards #-}

module Bar.Modules
    ( refreshXmonadInfo
    , refreshTime
    , refreshBattery
    , refreshUname
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, TVar, writeTVar)
import Control.Exception
import Control.Monad (forever)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)
import System.IO (hClose, hGetLine, hPrint, stderr, stdin)
import System.Process (runInteractiveCommand)

import Bar.APM

refreshXmonadInfo :: TVar String -> IO () -> IO ()
refreshXmonadInfo infoRef wake = do 
    info <- catch getLine $
        \(SomeException e) -> do
            hPrint stderr e
            return ""
    atomically $ writeTVar infoRef info
    wake
    refreshXmonadInfo infoRef wake

refreshTime :: TVar String -> IO () -> IO ()
refreshTime timeRef wake = do 
    zonedTime <- getZonedTime
    let time = formatTime defaultTimeLocale "%b. %d, %H:%M:%S" zonedTime
    atomically $ writeTVar timeRef time
    wake
    threadDelay 200000
    refreshTime timeRef wake

refreshBattery :: TVar String -> IO () -> IO ()
refreshBattery batteryRef wake = do
    APMPowerInfo{..} <- getPowerInfo
    let status = if apmACState == ACConnected then "+" else ""
        percent = show apmBatteryPercent ++ "%" ++ status
    atomically $ writeTVar batteryRef percent
    wake
    threadDelay 10000000
    refreshBattery batteryRef wake

refreshUname :: TVar String -> IO () -> IO ()
refreshUname unameRef wake = do 
    (hStdin, hStdout, hStderr, _) <- runInteractiveCommand "uname -r -s"
    hClose hStdin
    hClose hStderr
    uname <- hGetLine hStdout
    atomically $ writeTVar unameRef uname
    wake
