module Base.Random where

import           Control.Monad (replicateM)
import           Data.Char     (chr, isAlphaNum)
import           System.Random (getStdRandom, randomR)


constrain :: (Char -> Bool) -> IO Char -> IO Char
constrain p space = space >>= select
  where
    select x
      | p x       = return x
      | otherwise = constrain p space


asciiSpace :: IO Char
asciiSpace = getStdRandom $ randomR (chr 0,chr 127)


alphaNumSpace :: IO Char
alphaNumSpace = constrain isAlphaNum asciiSpace


randomString :: IO Char -> Int -> IO String
randomString = flip replicateM
