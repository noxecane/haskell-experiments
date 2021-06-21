{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
module Data.LaCarte
  ( (:<:)
  , pattern U
  , ShowUnit(..)
  , Unwrap(..)
  , Union(..)
  , Wrap(..)
  ) where

import Data.Void (Void)

class Union (fns :: [*]) where
  data Unit fns :: *

instance Union '[] where
  data Unit '[] = Empty Void

instance Union (f ': fs) where
  data Unit (f ': fs) = Head f
                      | Rest (Unit fs)

class Wrap (f :: *) (fs :: [*]) where
  wrap :: f -> Unit fs

instance Wrap f (f ': fs) where
  wrap = Head
  {-# INLINE wrap #-}

instance {-# OVERLAPPING #-} Wrap f fs => Wrap f (g ': fs) where
  wrap = Rest . wrap
  {-# INLINE wrap #-}

class Unwrap (f :: *) (fs :: [*]) where
  unwrap :: Unit fs -> Maybe f

instance Unwrap f (f ': fs) where
  unwrap (Head a) = Just a
  unwrap (Rest _) = Nothing
  {-# INLINE unwrap #-}

instance {-# OVERLAPPING #-} Unwrap f fs => Unwrap f (g ': fs) where
  unwrap (Head _)  = Nothing
  unwrap (Rest fs) = unwrap fs
  {-# INLINEABLE unwrap #-}

-- Group the constraints using "one of" constraint
class (Union fs, Wrap f fs, Unwrap f fs) => (f :: *) :<: (fs :: [*])
instance (Union fs, Wrap f fs, Unwrap f fs) => (f :<: fs)

class ShowUnit (as :: [*]) where
    showUnit :: Unit as -> String

instance ShowUnit '[] where
    showUnit _ = undefined

instance (Show a, ShowUnit as) => ShowUnit (a ': as) where
    showUnit (Head a) = show a
    showUnit (Rest as) = showUnit as

pattern U :: (a :<: as) => a -> Unit as
pattern U x <- (unwrap -> Just x)
  where U x = wrap x
