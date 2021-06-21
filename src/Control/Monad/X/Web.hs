{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.X.Web
  ( WebM, runWebM
  , WebConfig(..)
  , WebError(..)
  ) where

import Control.Exception          (Exception)

import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Except       (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Reader       (MonadReader, ReaderT, asks, runReaderT)

import Database.PostgreSQL.Simple (Connection)
import Opaleye.X                  (MonadDB(..), safeInsertOne, safeInsertOneReturningId, selectOne_, select_)

import Network.Mail.Mailjet       (ApiKey, MonadMailer(..), send_)

data WebConfig = WebConfig
  { getDBConn :: Connection
  , getMailKey :: ApiKey
  }

data WebError = forall e . Exception e => WebError e

newtype WebM a = WebM { getWebM :: ExceptT WebError (ReaderT WebConfig IO) a }
  deriving (Applicative, Functor, Monad, MonadError WebError, MonadReader WebConfig, MonadIO)

runWebM :: WebConfig -> WebM a -> IO (Either WebError a)
runWebM config  = flip runReaderT config . runExceptT . getWebM

instance MonadDB WebM where
  insertOne t h = do
    conn <- asks getDBConn
    result <- liftIO $ safeInsertOne conn t h
    errorOut result

  insertOneReturningId t h rId = do
    conn <- asks getDBConn
    result <- liftIO $ safeInsertOneReturningId conn t h rId
    errorOut result

  selectOne sel = asks getDBConn >>= liftIO . flip selectOne_ sel

  select sel = asks getDBConn >>= liftIO . flip select_ sel

instance MonadMailer WebM where
  sendMails mails = do
    key <- asks getMailKey
    result <- liftIO $ send_ key mails
    errorOut result

errorOut :: (MonadError WebError m, Exception e) => Either e a -> m a
errorOut (Left e) = throwError $ WebError e
errorOut (Right a)  = return a
