{-# LANGUAGE OverloadedStrings #-}
{-
So this is copy paste from this https://github.com/parsonsmatt/servant-persistent/blob/master/src/DevelMain.hs repository.
Basically it enables hotreloading together with the "make run" command at the Makefile
-}
module DevMain where

import           Prelude

import           Control.Concurrent             ( MVar
                                                , ThreadId
                                                , forkIO
                                                , killThread
                                                , newEmptyMVar
                                                , putMVar
                                                , takeMVar
                                                )
import           Control.Exception              ( finally )
import           Control.Monad                  ( (>=>) )
import           Data.IORef                     ( IORef
                                                , newIORef
                                                , readIORef
                                                , writeIORef
                                                )
import           Foreign.Store                  ( Store(..)
                                                , lookupStore
                                                , readStore
                                                , storeAction
                                                , withStore
                                                )
import           GHC.Word                       ( Word32 )
import           Main                           ( runApp )

update :: IO ()
update = do
  mtidStore <- lookupStore tidStoreNum
  case mtidStore of
    Nothing -> do
      done <- storeAction doneStore newEmptyMVar
      tid  <- start done
      _    <- storeAction (Store tidStoreNum) (newIORef tid)
      return ()
    Just tidStore -> restartAppInNewThread tidStore
 where
  doneStore :: Store (MVar ())
  doneStore = Store 0

  restartAppInNewThread :: Store (IORef ThreadId) -> IO ()
  restartAppInNewThread tidStore = modifyStoredIORef tidStore $ \tid -> do
    killThread tid
    withStore doneStore takeMVar
    readStore doneStore >>= start


  start
    :: MVar () -- ^ Written to when the thread is killed.
    -> IO ThreadId
  start done = forkIO (finally runApp (putMVar done ()))

shutdown :: IO ()
shutdown = do
  mtidStore <- lookupStore tidStoreNum
  case mtidStore of
    Nothing       -> putStrLn "no app running"
    Just tidStore -> do
      withStore tidStore $ readIORef >=> killThread
      putStrLn "App is shutdown"

tidStoreNum :: Word32
tidStoreNum = 1

modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO ()
modifyStoredIORef store f = withStore store $ \ref -> do
  v <- readIORef ref
  f v >>= writeIORef ref
