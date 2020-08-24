{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Database.PostgreSQL.Tx.Simple
  ( PgSimpleEnv
  , PgSimpleM
  , module Database.PostgreSQL.Tx.Simple
  ) where

import Data.Int (Int64)
import Database.PostgreSQL.Tx (TxM)
import Database.PostgreSQL.Tx.Simple.Internal
import qualified Database.PostgreSQL.Simple as Simple
import qualified Database.PostgreSQL.Simple.Transaction as Simple

-- | Analogue of 'Simple.withTransaction'.
--
-- @since 0.1.0.0
withTransaction :: (PgSimpleEnv r) => r -> TxM r a -> IO a
withTransaction = unsafeRunTransaction Simple.withTransaction

-- | Analogue of 'Simple.withTransactionLevel'.
--
-- @since 0.1.0.0
withTransactionLevel :: (PgSimpleEnv r) => Simple.IsolationLevel -> r -> TxM r a -> IO a
withTransactionLevel = unsafeRunTransaction . Simple.withTransactionLevel

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
