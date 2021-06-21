{-# LANGUAGE OverloadedStrings #-}
module Base.WebSockets where

import           Control.Concurrent     (threadDelay)
import           Control.Concurrent.STM (TBQueue, atomically, writeTBQueue)
import           Data.Aeson             (FromJSON, decode, decode')
import           Data.ByteString        (ByteString)
import           Network.WebSockets     (ClientApp, Connection, WebSocketsData,
                                         forkPingThread, receiveData, sendClose)

proxy :: WebSocketsData a => (a -> IO b) -> TBQueue b -> ClientApp ()
proxy mapper queue conn = do
  forkPingThread conn 30
  _ <- loop
  sendClose conn ("Closing Client" :: ByteString)
  where
    loop = do
      msg <- receiveData conn
      result <- mapper msg
      atomically $ writeTBQueue queue result
      loop

sampleProxy :: Int -> (ByteString -> a) -> TBQueue a -> IO ()
sampleProxy 0 _ _  = return ()
sampleProxy n mapper queue = do
  atomically $ writeTBQueue queue (mapper "Message")
  threadDelay 3000000
  sampleProxy (n - 1) mapper queue

receiveJson :: FromJSON a => Connection -> IO (Maybe a)
receiveJson = (decode <$>) . receiveData

receiveJson' :: FromJSON a => Connection -> IO (Maybe a)
receiveJson' = (decode' <$>) . receiveData
