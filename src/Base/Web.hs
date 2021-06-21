{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Base.Web where

import           Base.ALaCarte              ((:+:), (:<:), inject)
import           Base.Opaleye               (ConstraintError, MonadDB (..))
import           Control.Monad.Except       (ExceptT (..), MonadError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader (..), ReaderT)
import           Database.PostgreSQL.Simple (Connection)

import qualified Base.Opaleye               as O
import qualified Control.Monad.Except       as E
import qualified Control.Monad.Reader       as R
import qualified Database.Redis             as Redis


data WebConfig = WebConfig
  { dbConn    :: Connection
  , redisConn :: Redis.Connection
  }

data RedisError

type WebError = RedisError :+: ConstraintError

newtype WebM es a = WebM { getWebM :: ExceptT es (ReaderT WebConfig IO) a }
  deriving (Applicative, Functor, Monad, MonadReader WebConfig, MonadError es, MonadIO)


runWebM :: WebConfig -> WebM es a -> IO (Either es a)
runWebM config = flip R.runReaderT config . E.runExceptT . getWebM

throwCoError :: (MonadError es m, e :<: es) => e -> m a
throwCoError = E.throwError . inject

liftEither :: (MonadError es m, e :<: es) => Either e a -> m a
liftEither = either throwCoError return

instance (ConstraintError :<: es) => MonadDB (WebM es) where
  insertOne t h = do
    conn <- R.asks dbConn
    result <- liftIO $ O.safeInsertOne conn t h
    liftEither result

  insertOneReturningId t h rId = do
    conn <- R.asks dbConn
    result <- liftIO $ O.safeInsertOneReturning conn t h rId
    liftEither result

  selectOne sel = R.asks dbConn >>= liftIO . flip O.selectOne_ sel

  select sel = R.asks dbConn >>= liftIO . flip O.select_ sel
