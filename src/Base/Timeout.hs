module Base.Timeout where

import           System.Timeout (timeout)

retry :: Int -> Int -> IO a -> IO (Maybe a)
retry 0 _ _ = return Nothing
retry retries microseconds action = do
    result <- timeout microseconds action
    case result of
        Nothing -> retry (retries-1) microseconds action
        Just a  -> return (Just a)
