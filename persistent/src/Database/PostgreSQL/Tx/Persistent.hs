{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Database.PostgreSQL.Tx.Persistent where

import Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr)
import Control.Monad.Reader (runReaderT)
import Data.Kind (Constraint)
import Data.Proxy (Proxy(Proxy))
import Database.PostgreSQL.Tx (TxEnv, TxM)
import Database.PostgreSQL.Tx.Unsafe (unsafeMksTxM)
import qualified Database.Persist.Postgresql as Persist

type Logger = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

type PgPersistEnv r = (TxEnv r Persist.SqlBackend) :: Constraint

type PgPersistM a = forall r. (PgPersistEnv r) => TxM r a

unsafePgPersistIO :: Persist.SqlPersistT IO a -> PgPersistM a
unsafePgPersistIO x = do
  unsafeMksTxM \sqlBackend -> do
    runReaderT x sqlBackend

unsafePgPersistIO1 :: (x1 -> Persist.SqlPersistT IO a) -> x1 -> PgPersistM a
unsafePgPersistIO1 f x1 = unsafePgPersistIO $ f x1

-- | Analogue of 'Persist.get'
get
  :: (Persist.PersistRecordBackend record Persist.SqlBackend)
  => Persist.Key record
  -> PgPersistM (Maybe record)
get = unsafePgPersistIO1 Persist.get

-- | Analogue of 'Persist.getJust'
getJust
  :: ( Persist.PersistStoreRead Persist.SqlBackend
     , Show (Persist.Key record)
     , Persist.PersistRecordBackend record Persist.SqlBackend
     )
  => Persist.Key record -> PgPersistM record
getJust = unsafePgPersistIO1 Persist.getJust
