{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Database.PostgreSQL.Tx.Simple.Internal
  ( -- * Disclaimer
    -- $disclaimer

    -- ** Internals
    module Database.PostgreSQL.Tx.Simple.Internal
  ) where

import Data.Kind (Constraint)
import Database.PostgreSQL.Tx (TxEnvs, TxException, TxM, askTxEnv, mapExceptionTx)
import Database.PostgreSQL.Tx.LibPQ.Connection (LibPQConnection)
import Database.PostgreSQL.Tx.Simple.Connection (PgSimpleConnection(unsafeGetPgSimpleConnection))
import Database.PostgreSQL.Tx.Unsafe (unsafeMkTxException, unsafeMksTxM, unsafeRunIOInTxM, unsafeRunTxM)
import qualified Data.ByteString.Char8 as Char8
import qualified Database.PostgreSQL.Simple as Simple

-- | Runtime environment needed to run @postgresql-simple@ via @postgresql-tx@.
--
-- @since 0.2.0.0
type PgSimpleEnv r =
  ( TxEnvs
      '[ PgSimpleConnection
       , LibPQConnection
       ] r
  ) :: Constraint

-- | Monad type alias for running @postgresql-simple@ via @postgresql-tx@.
--
-- @since 0.2.0.0
type PgSimpleM a = forall r. (PgSimpleEnv r) => TxM r a

unsafeRunTransaction
  :: (PgSimpleEnv r)
  => (Simple.Connection -> IO a -> IO a)
  -> r -> TxM r a -> IO a
unsafeRunTransaction f r x = do
  unsafeRunTxM r do
    conn <- unsafeGetPgSimpleConnection <$> askTxEnv
    unsafeRunIOInTxM $ f conn (unsafeRunTxM r x)

fromSqlError :: Simple.SqlError -> TxException
fromSqlError = unsafeMkTxException (Just . Char8.unpack . Simple.sqlState)

unsafeFromPgSimple
  :: (Simple.Connection -> IO x)
  -> PgSimpleM x
unsafeFromPgSimple f =
  mapExceptionTx (Just . fromSqlError) do
    unsafeMksTxM (f . unsafeGetPgSimpleConnection)

unsafeFromPgSimple1
  :: (Simple.Connection -> a1 -> IO x)
  -> a1 -> PgSimpleM x
unsafeFromPgSimple1 f a1 = unsafeFromPgSimple \c -> f c a1

unsafeFromPgSimple2
  :: (Simple.Connection -> a1 -> a2 -> IO x)
  -> a1 -> a2 -> PgSimpleM x
unsafeFromPgSimple2 f a1 a2 = unsafeFromPgSimple \c -> f c a1 a2

-- $disclaimer
--
-- Changes to this module will not be reflected in the library's version
-- updates.
