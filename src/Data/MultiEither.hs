{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
module Data.MultiEither
  ( MultiEither(..)
  , pattern MultiU
  , liftEither
  , multiEither
  , multiLeft
  ) where

import Data.LaCarte (ShowUnit(..), Unit, (:<:), wrap, unwrap)

pattern MultiU :: (a :<: as) => a -> MultiEither as b
pattern MultiU x <- MultiLeft (unwrap -> Just x)
  where MultiU x = MultiLeft $ wrap x

data MultiEither (errs :: [*]) (a :: *) where
  MultiLeft :: Unit errs -> MultiEither errs a
  SingletonRight :: a -> MultiEither errs a

instance (Show a, ShowUnit errs) => Show (MultiEither errs a) where
  show (SingletonRight a) = "SingletonRight " ++ show a
  show (MultiLeft err)    = "MultifLeft " ++ showUnit err

instance Functor (MultiEither errs) where
   fmap f (SingletonRight a) = SingletonRight (f a)
   fmap _ (MultiLeft err)    = MultiLeft err
   {-# INLINE fmap #-}

instance Semigroup (MultiEither errs a) where
  MultiLeft _ <> b = b
  a <> _           = a

instance Applicative (MultiEither errs) where
    pure = SingletonRight
    {-# INLINE pure #-}

    MultiLeft err  <*> _   = MultiLeft err
    SingletonRight f <*> x = fmap f x
    {-# INLINE (<*>) #-}

instance Monad (MultiEither errs) where
    return = pure
    {-# INLINE return #-}

    SingletonRight a  >>= f = f a
    MultiLeft err >>= _     = MultiLeft err
    {-# INLINEABLE (>>=) #-}

multiLeft :: (e :<: errs) => e -> MultiEither errs a
multiLeft = MultiLeft . wrap
{-# INLINE multiLeft #-}

liftEither :: (e :<: errs) => Either e b -> MultiEither errs b
liftEither (Left err) = multiLeft err
liftEither (Right a)  = SingletonRight a
{-# INLINE liftEither #-}

multiEither :: (Unit errs -> b) -> (a -> b) -> MultiEither errs a -> b
multiEither l r me = case me of
  SingletonRight a -> r a
  MultiLeft ue     -> l ue
