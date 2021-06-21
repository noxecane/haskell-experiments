module MyChecked where

howFast :: (Fractional a, Floating a) => a -> a -> a -> a
howFast i f n = (f / i) ** (1 / (n - 1))

howMuch :: (Fractional a, Floating a) => a -> a -> a -> a
howMuch i k n = i * (k ** (n - 1))
