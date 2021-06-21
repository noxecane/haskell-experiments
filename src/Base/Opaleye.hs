{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Base.Opaleye
  ( module Base.Opaleye.Alias
  , MonadDB(..)
  , ConstraintError(..)
  , safeInsertOne
  , safeInsertOneReturning
  , safeInsertOneReturningId'
  , select_
  , selectOne_
  , selectToString
  , updateOne
  ) where

import           Base.Opaleye.Alias
import           Control.Exception               (throwIO)
import           Control.Monad.Catch             (catch)
import           Data.Int                        (Int64)
import           Data.Maybe                      (fromJust, fromMaybe,
                                                  listToMaybe)
import           Data.Profunctor.Product.Default (Default)
import           Data.Text                       (Text)
import           Data.Text.Encoding              (decodeUtf8)
import           Database.PostgreSQL.Simple      (Connection, SqlError (..))
import           Opaleye
import           Opaleye.Internal.Manipulation   (Updater)


class (Monad m) => MonadDB m where
   insertOne :: Default Constant hW pW => Table pW pR -> hW -> m Int64
   insertOneReturningId ::
     (Default Constant hW pW, QueryRunnerColumnDefault a a)
     => Table pW pR -> (pR -> Column a) -> hW -> m (Maybe a)
   selectOne :: (Default FromFields a b) => Select a -> m (Maybe b)
   select :: (Default FromFields a b) => Select a -> m [b]


newtype ConstraintError = UniqueContraintError Text
  deriving (Show)


selectToString :: Default Unpackspec a a => Select a -> String
selectToString = fromMaybe "Empty query" . showSqlForPostgres


selectOne_ :: (Default FromFields a b) => Connection -> Select a -> IO (Maybe b)
selectOne_ conn = fmap listToMaybe . runSelect conn . limit 1


select_ :: (Default FromFields a b) => Connection -> Select a -> IO [b]
select_ = runSelect


updateOne ::
  (Default Updater pR pW, Default QueryRunner a b) =>
  Connection -> Table pW pR -> (pR -> pR) ->
  (pR -> Column PGBool) -> (pR -> a) -> IO (Maybe b)
updateOne conn t x y z = listToMaybe <$> runUpdate_ conn Update
  { uTable = t
  , uUpdateWith = updateEasy x
  , uWhere = y
  , uReturning = rReturning z
  }

safeInsertOne ::
     Default Constant hW pW
  => Connection
  -> Table pW pR
  -> hW
  -> IO (Either ConstraintError Int64)
safeInsertOne conn t h = runInsertSafe conn Insert
  { iTable = t
  , iRows = [toFields h]
  , iReturning  = rCount
  , iOnConflict = Nothing
  }


safeInsertOneReturning ::
  (Default Constant hW pW, Default QueryRunner a b)
  => Connection -> Table pW pR -> (pR -> a) -> hW -> IO (Either ConstraintError (Maybe b))
safeInsertOneReturning conn t rId h = fmap listToMaybe <$> runInsertSafe conn Insert
  { iTable = t
  , iRows = [toFields h]
  , iReturning  = rReturning rId
  , iOnConflict = Nothing
  }


safeInsertOneReturningId' ::
  (Default Constant hW pW, Default QueryRunner a b)
  => Connection -> Table pW pR -> (pR -> a) -> hW -> IO (Either ConstraintError b)
safeInsertOneReturningId' conn t rId = fmap (fromJust <$>) . safeInsertOneReturning conn t rId


runInsertSafe :: Connection -> Insert a -> IO (Either ConstraintError a)
runInsertSafe conn = flip catch handleConstraintErrors  . correctInsert
  where
    correctInsert = fmap Right . runInsert_ conn


handleConstraintErrors :: SqlError -> IO (Either ConstraintError a)
handleConstraintErrors err@SqlError{ sqlState, sqlErrorDetail } =
  let errorMessage = decodeUtf8 sqlErrorDetail
      reportError  = return . Left
  in  case sqlState of
        "23505" -> reportError $ UniqueContraintError errorMessage
        _       -> throwIO err
