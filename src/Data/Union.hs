{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Union where

data Union (as :: [*]) where
  Union :: Either (Union as) a -> Union (a ': as)

-- Extend an existing union to support a new type a at the head
-- of the list of types.
unionLeft :: Union as -> Union (a ': as)
{-# INLINE unionLeft #-}
unionLeft u = Union (Left u)

-- Wrap a value in a union as long as it's the first type in the list
-- of types
unionRight :: a -> Union (a ': as)
{-# INLINE unionRight #-}
unionRight a = Union (Right a)

matchRight :: Union (a ': as) -> Maybe a
matchRight (Union (Left _))  = Nothing
matchRight (Union (Right x)) = Just x

pattern U :: (Unwrap a as, Wrap a as) => a -> Union as
pattern U x <- (unwrapVal -> Just x)
  where U x = wrapVal x

class Member (a :: *) (as :: [*])

instance Member a (a ': as)
instance Member b (a ': as)

class Wrap a as where
  wrapVal :: a -> Union as

instance Wrap a (a ': as) where
  wrapVal = unionRight
  {-# INLINE wrapVal #-}

-- Lifting into one of the tail of as
instance Wrap b as => Wrap b (a ': as) where
  -- TODO: Deal with the issues of n wrapping lefts
  -- This recursively calls unionLeft to keep adding all types to the Union
  wrapVal = unionLeft . wrapVal
  {-# INLINE wrapVal #-}

class Unwrap a as where
  unwrapVal :: Union as -> Maybe a

instance Unwrap a (a ': as) where
  unwrapVal (Union (Right a)) = Just a
  unwrapVal (Union (Left _))  = Nothing
  {-# INLINE unwrapVal #-}

wrapped :: Union '[Int, Bool]
wrapped = wrapVal False

simple :: Union '[Int, Bool]
simple = wrapVal (4 :: Int)

whatitis :: IO ()
whatitis = case wrapped of
  U (_ :: Int)  -> print "It's a number"
  U (_ :: Bool) -> print "It's some bool"
  _             -> undefined

instance {-# OVERLAPPABLE #-} Unwrap b as => Unwrap b (a ': as) where
  unwrapVal (Union (Left u))   = unwrapVal u
  unwrapVal (Union (Right _))  = Nothing

class ShowUnion types where
    showUnion :: Union types -> String

instance ShowUnion '[] where
    showUnion _ = undefined

instance (Show a, ShowUnion as) => ShowUnion (a ': as) where
    showUnion (Union (Right a)) = show a
    showUnion (Union (Left u))  = showUnion u
