{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.PostgreSQL.Tx.Query
  ( PgQueryEnv
  , PgQueryM
  , Logger
  , module Database.PostgreSQL.Tx.Query
  , module Database.PostgreSQL.Tx.Query.Internal.Reexport
  ) where

import Data.Int (Int64)
import Database.PostgreSQL.Tx (TxM)
import Database.PostgreSQL.Tx.Query.Internal
import Database.PostgreSQL.Tx.Query.Internal.Reexport
import qualified Database.PostgreSQL.Query as Query
import qualified Database.PostgreSQL.Simple.Transaction as Simple

-- | Analogue of 'Query.pgWithTransaction'.
--
-- @since 0.1.0.0
pgWithTransaction :: (PgQueryEnv r) => r -> TxM r a -> IO a
pgWithTransaction = unsafeRunPgQueryTransaction Query.pgWithTransaction

-- | Analogue of 'Query.pgWithTransactionMode'.
--
-- @since 0.1.0.0
pgWithTransactionMode :: (PgQueryEnv r) => Simple.TransactionMode -> r -> TxM r a -> IO a
pgWithTransactionMode m = unsafeRunPgQueryTransaction (Query.pgWithTransactionMode m)

-- | Analogue of 'Query.pgQuery'.
--
-- @since 0.1.0.0
pgQuery :: (ToSqlBuilder q, FromRow r) => q -> PgQueryM [r]
pgQuery = unsafeFromPgQueryIO . Query.pgQuery

-- | Analogue of 'Query.pgExecute'.
--
-- @since 0.1.0.0
pgExecute :: (ToSqlBuilder q) => q -> PgQueryM Int64
pgExecute = unsafeFromPgQueryIO . Query.pgExecute
