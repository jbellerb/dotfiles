{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Bar.APM
Copyright   :  (c) Jared Beller 2020-2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  non-portable (requires OpenBSD)
-}

module Bar.APM
    ( APMPowerInfo(..)
    , BatteryState(..)
    , ACState(..)
    , getPowerInfo
    ) where

import Foreign (Storable)
import System.IO (openFile, IOMode(..))
import System.Posix.IO (closeFd, handleToFd)
import System.Posix.IOCtl

#include <sys/types.h>
#include <machine/apmvar.h>

getPowerInfo :: IO APMPowerInfo
getPowerInfo = do
    apm_fd <- handleToFd =<< openFile "/dev/apm" ReadMode
    power_info <- ioctl' apm_fd APM_IOC_GETPOWER
    closeFd apm_fd
    return power_info

data APM_IOC_GETPOWER = APM_IOC_GETPOWER

instance IOControl APM_IOC_GETPOWER APMPowerInfo where
    ioctlReq _ = 1075855619 -- C2HS had trouble expanding this

{#enum define BatteryState
    { APM_BATT_HIGH as BattHigh
    , APM_BATT_LOW as BattLow
    , APM_BATT_CRITICAL as BattCritical
    , APM_BATT_CHARGING as BattCharging
--    , APM_BATT_ABSENT as BattAbsent
    , APM_BATT_UNKNOWN as BattUnknown
    } deriving (Eq) #}

{#enum define ACState
    { APM_AC_OFF as ACDisconnected
    , APM_AC_ON as ACConnected
    , APM_AC_BACKUP as ACBackup
    , APM_AC_UNKNOWN as ACUnknown
    } deriving (Eq) #}

data APMPowerInfo = APMPowerInfo
    { apmBatteryState :: BatteryState
    , apmACState :: ACState
    , apmBatteryPercent :: Int
    , apmMinutesLeft :: Int
    }

instance Storable APMPowerInfo where
    sizeOf _ = {#sizeof apm_power_info #}
    alignment _ = {#alignof apm_power_info #}

    peek p = do
        batteryState <- {#get apm_power_info->battery_state #} p
        acState <- {#get apm_power_info->ac_state #} p
        batteryPercent <- {#get apm_power_info->battery_life #} p
        minutesLeft <- {#get apm_power_info->minutes_left #} p
        return $ APMPowerInfo
            (cToEnum batteryState)
            (cToEnum acState)
            (fromIntegral batteryPercent)
            (fromIntegral minutesLeft)

    poke p APMPowerInfo{..} = do
        {#set apm_power_info.battery_state #} p $ cFromEnum apmBatteryState
        {#set apm_power_info.ac_state #} p $ cFromEnum apmACState
        {#set apm_power_info.battery_life #} p $ fromIntegral apmBatteryPercent
        {#set apm_power_info.minutes_left #} p $ fromIntegral apmMinutesLeft

cToEnum :: (Integral i, Enum e) => i -> e
cToEnum = toEnum . fromIntegral

cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum = fromIntegral . fromEnum
