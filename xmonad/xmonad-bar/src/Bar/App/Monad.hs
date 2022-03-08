{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      :  Bar.App.Monad
Copyright   :  (c) Jared Beller 2022
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable
-}

module Bar.App.Monad
    ( -- * Main application monad
      App(..)
    , runApp
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)

import Bar.App.Env (Env)

newtype App a = App
    { unApp :: ReaderT Env IO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

runApp :: Env -> App a -> IO a
runApp env = flip runReaderT env . unApp
