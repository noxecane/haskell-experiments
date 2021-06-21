{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
module Control.Monad.MultiExceptT where

import Control.Monad             (liftM2)
import Control.Monad.IO.Class    (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))

import Data.LaCarte              ((:<:))
import Data.MultiEither          (MultiEither(..), multiEither, multiLeft)

newtype MultiExceptT (errs :: [*]) (m :: * -> *) (a :: *) =
  MultiExceptT (m (MultiEither errs a))

runMultiExceptT :: MultiExceptT errs m a  -> m (MultiEither errs a)
runMultiExceptT (MultiExceptT m) = m
{-# INLINE runMultiExceptT #-}

instance Functor m => Functor (MultiExceptT errs m) where
  fmap f = MultiExceptT . fmap (fmap f) . runMultiExceptT
  {-# INLINE fmap #-}

instance Monad m => Applicative (MultiExceptT errs m) where
  pure = MultiExceptT . return . SingletonRight
  {-# INLINE pure #-}

  MultiExceptT mF <*> MultiExceptT mV = MultiExceptT $ liftM2 multiEitherApply mF mV
    where multiEitherApply a b = a <*> b
  {-# INLINEABLE (<*>) #-}

instance Monad m => Monad (MultiExceptT errs m) where
  return = pure
  MultiExceptT m  >>= f = prefixBind m
    where
      prefixBind = MultiExceptT . (>>= innerResolve)
      innerResolve = multiEither returnFailure  (runMultiExceptT . f)
      returnFailure = return . MultiLeft

instance MonadTrans (MultiExceptT errs) where
  lift = MultiExceptT . fmap SingletonRight

instance MonadIO m => MonadIO (MultiExceptT errs m) where
  liftIO = lift . liftIO

throwE :: (e :<: errs, Monad m) => e -> MultiExceptT errs m a
throwE = MultiExceptT . return . multiLeft

data N = N
data O = O
data P = P

getMoney :: (N :<: errs) => MultiExceptT errs IO Int
getMoney = throwE N

getAge :: (O :<: errs) => MultiExceptT errs IO Int
getAge = throwE O

getName :: (P :<: errs) => MultiExceptT errs IO String
getName = throwE P

compute :: MultiExceptT '[O, P, N] IO String
compute = do
  m <- getMoney
  a <- getAge
  n <- getName
  return $ n ++ show (a + m)
