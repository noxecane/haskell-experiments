{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Exception.Checked where

import Control.Exception         (Exception(..), SomeException)
import Control.Monad             (liftM2)
import Control.Monad.Reader      (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Either               (either)

class Exception e => Throws e p
data Caught e p
instance Exception e => Throws e (Caught e p) -- Caught e p removes e
instance Throws e p => Throws e (Caught e1 p) -- e1 remains
instance Exception e => Throws e (Caught SomeException p) -- SomeException catches all


newtype CheckedT p m a = CheckedT { unCheckedT :: m (Either SomeException a) }

newtype WebM a = WebM { getWebM :: ReaderT String IO a }
  deriving (Applicative, Functor, Monad, MonadReader String)

newtype ErrorM p a = ErrorM { getErrorM :: CheckedT p WebM a }

throwCheckedT :: (Monad m, Throws e p) => e -> CheckedT p m a
throwCheckedT = CheckedT . return . Left . toException

catchCheckedT
  :: (Exception e, Monad m)
  => CheckedT p m a
  -> (e -> CheckedT (Caught e p) m a)
  -> CheckedT p m a
catchCheckedT (CheckedT m) h = CheckedT $ do
  eV <- m
  case eV of
    Right v -> return $ Right v
    Left e  -> case fromException e of
      Just et -> unCheckedT $ h et
      Nothing -> return $ Left e

runCheckedT :: Monad m  => CheckedT p m a -> m a
runCheckedT (CheckedT m) = fmap fromRight m
  where
    fromRight (Right x) = x
    fromRight (Left _)  = error "Control.Exception.Checked: Checked exception escaped"

instance Monad m => Functor (CheckedT p m) where
  fmap f (CheckedT m) = CheckedT $ fmap f <$> m

instance Monad m => Applicative (CheckedT p m) where
  pure = return
  CheckedT mF <*> CheckedT mA = CheckedT $ liftM2 eitherApply mF mA
    where eitherApply a b = a <*> b

instance Monad m => Monad (CheckedT p m) where
  return = CheckedT . return . Right
  CheckedT m  >>= f = prefixBind m
    where
      prefixBind = CheckedT . (>>= innerResolve)
      innerResolve = either returnFailure (unCheckedT . f)
      returnFailure = return . Left

instance MonadTrans (CheckedT p) where
  lift = CheckedT . fmap Right
