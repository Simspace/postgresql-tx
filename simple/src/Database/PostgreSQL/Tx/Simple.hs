{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Database.PostgreSQL.Tx.Simple
  ( PgSimpleEnv
  , PgSimpleM
  , module Database.PostgreSQL.Tx.Simple
  , module Database.PostgreSQL.Tx.Simple.Connection
  , module Database.PostgreSQL.Tx.LibPQ.Connection
  ) where

import Control.Exception (Exception)
import Data.Int (Int64)
import Database.PostgreSQL.Tx (TxM, shouldRetryTx)
import Database.PostgreSQL.Tx.LibPQ.Connection
import Database.PostgreSQL.Tx.Simple.Internal
import Database.PostgreSQL.Tx.Simple.Connection
import GHC.Stack (HasCallStack)
import qualified Database.PostgreSQL.Simple as Simple
import qualified Database.PostgreSQL.Simple.Transaction as Simple

-- | Analogue of 'Simple.withTransaction'.
--
-- @since 0.1.0.0
withTransaction
  :: (PgSimpleEnv r, HasCallStack)
  => r -> (HasCallStack => TxM r a) -> IO a
withTransaction = unsafeRunTransaction Simple.withTransaction

-- | Analogue of 'Simple.withTransactionLevel'.
--
-- @since 0.1.0.0
withTransactionLevel
  :: (PgSimpleEnv r, HasCallStack)
  => Simple.IsolationLevel -> r -> (HasCallStack => TxM r a) -> IO a
withTransactionLevel = unsafeRunTransaction . Simple.withTransactionLevel

-- | Analogue of 'Simple.withTransactionMode'.
--
-- @since 0.2.0.0
withTransactionMode
  :: (PgSimpleEnv r, HasCallStack)
  => Simple.TransactionMode -> r -> (HasCallStack => TxM r a) -> IO a
withTransactionMode = unsafeRunTransaction . Simple.withTransactionMode

-- | Analogue of 'Simple.withTransactionSerializable'.
-- Unlike @postgresql-simple@, uses 'shouldRetryTx' to also retry
-- on @deadlock_detected@, not just @serialization_failure@.
--
-- Note that any 'IO' that occurs inside the 'TxM' may be executed multiple times.
--
-- @since 0.2.0.0
withTransactionSerializable
  :: (PgSimpleEnv r, HasCallStack)
  => r -> (HasCallStack => TxM r a) -> IO a
withTransactionSerializable =
  withTransactionModeRetry mode shouldRetryTx
  where
  mode = Simple.TransactionMode Simple.Serializable Simple.ReadWrite

-- | Analogue of 'Simple.withTransactionModeRetry'.
-- You should generally prefer 'withTransactionSerializable'.
--
-- Note that any 'IO' that occurs inside the 'TxM' may be executed multiple times.
--
-- @since 0.2.0.0
withTransactionModeRetry
  :: (Exception e, PgSimpleEnv r, HasCallStack)
  => Simple.TransactionMode -> (e -> Bool) -> r -> (HasCallStack => TxM r a) -> IO a
withTransactionModeRetry mode shouldRetry =
  unsafeRunTransaction $ Simple.withTransactionModeRetry' mode shouldRetry

-- | Analogue of 'Simple.query'
--
-- @since 0.1.0.0
query :: (Simple.ToRow q, Simple.FromRow x) => Simple.Query -> q -> PgSimpleM [x]
query = unsafeFromPgSimple2 Simple.query

-- | Analogue of 'Simple.query_'.
--
-- @since 0.1.0.0
query_ :: (Simple.FromRow x) => Simple.Query -> PgSimpleM [x]
query_ = unsafeFromPgSimple1 Simple.query_

-- | Analogue of 'Simple.execute'.
--
-- @since 0.1.0.0
execute :: (Simple.ToRow q) => Simple.Query -> q -> PgSimpleM Int64
execute = unsafeFromPgSimple2 Simple.execute

-- | Analogue of 'Simple.execute_'.
--
-- @since 0.1.0.0
execute_ :: Simple.Query -> PgSimpleM Int64
execute_ = unsafeFromPgSimple1 Simple.execute_
