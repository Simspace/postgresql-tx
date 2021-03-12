{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.PostgreSQL.Tx.Query
  ( PgQueryEnv
  , PgQueryM
  , module Database.PostgreSQL.Tx.Query
  , module Database.PostgreSQL.Tx.Query.Internal.Reexport
  , module Database.PostgreSQL.Tx.Simple.Connection
  , module Database.PostgreSQL.Tx.MonadLogger
  ) where

import Control.Exception (Exception)
import Data.Int (Int64)
import Database.PostgreSQL.Tx (TxM)
import Database.PostgreSQL.Tx.MonadLogger
import Database.PostgreSQL.Tx.Query.Internal
import Database.PostgreSQL.Tx.Query.Internal.Reexport
import Database.PostgreSQL.Tx.Simple.Connection
import GHC.Stack (HasCallStack)
import qualified Database.PostgreSQL.Query as Query
import qualified Database.PostgreSQL.Simple.Transaction as Simple
import qualified Database.PostgreSQL.Tx.Simple as Tx.Simple

-- | Analogue of 'Query.pgWithTransaction'.
--
-- @since 0.1.0.0
pgWithTransaction
  :: (PgQueryEnv r, HasCallStack)
  => r -> (HasCallStack => TxM r a) -> IO a
pgWithTransaction = unsafeRunPgQueryTransaction Query.pgWithTransaction

-- | Analogue of 'Query.pgWithTransactionMode'.
--
-- @since 0.1.0.0
pgWithTransactionMode
  :: (PgQueryEnv r, HasCallStack)
  => Simple.TransactionMode -> r -> (HasCallStack => TxM r a) -> IO a
pgWithTransactionMode m = unsafeRunPgQueryTransaction (Query.pgWithTransactionMode m)

-- | Analogue of 'Query.pgWithTransactionSerializable'
-- Unlike @postgresql-query@, uses 'shouldRetryTx' to also retry
-- on @deadlock_detected@, not just @serialization_failure@.
--
-- Note that any 'IO' that occurs inside the 'TxM' may be executed multiple times.
--
-- @since 0.2.0.0
pgWithTransactionSerializable
  :: (PgQueryEnv r, HasCallStack)
  => r -> (HasCallStack => TxM r a) -> IO a
pgWithTransactionSerializable = Tx.Simple.withTransactionSerializable

-- | Analogue of 'Query.pgWithTransactionModeRetry'.
-- You should generally prefer 'pgWithTransactionSerializable'.
--
-- Note that any 'IO' that occurs inside the 'TxM' may be executed multiple times.
--
-- @since 0.2.0.0
pgWithTransactionModeRetry
  :: (PgQueryEnv r, Exception e, HasCallStack)
  => Simple.TransactionMode -> (e -> Bool) -> r -> (HasCallStack => TxM r a) -> IO a
pgWithTransactionModeRetry = Tx.Simple.withTransactionModeRetry

-- | Analogue of 'Query.pgWithSavepoint'.
--
-- @since 0.2.0.0
pgWithSavepoint :: (HasCallStack => PgQueryM a) -> PgQueryM a
pgWithSavepoint x =
  unsafeFromPgQueryIO
    $ Query.pgWithSavepoint
    $ unsafeToPgQueryIO x

-- | Analogue of 'Query.pgQuery'.
--
-- @since 0.1.0.0
pgQuery :: (ToSqlBuilder q, FromRow r, HasCallStack) => q -> PgQueryM [r]
pgQuery = unsafeFromPgQueryIO . Query.pgQuery

-- | Analogue of 'Query.pgQueryWithMasker'.
--
-- @since 0.2.0.0
pgQueryWithMasker :: (ToSqlBuilder q, FromRow r, HasCallStack) => LogMasker -> q -> PgQueryM [r]
pgQueryWithMasker masker = unsafeFromPgQueryIO . Query.pgQueryWithMasker masker

-- | Analogue of 'Query.pgExecute'.
--
-- @since 0.1.0.0
pgExecute :: (ToSqlBuilder q, HasCallStack) => q -> PgQueryM Int64
pgExecute = unsafeFromPgQueryIO . Query.pgExecute

-- | Analogue of 'Query.pgExecuteWithMasker'.
--
-- @since 0.2.0.0
pgExecuteWithMasker :: (ToSqlBuilder q, HasCallStack) => LogMasker -> q -> PgQueryM Int64
pgExecuteWithMasker masker = unsafeFromPgQueryIO . Query.pgExecuteWithMasker masker
