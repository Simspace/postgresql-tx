{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Database.PostgreSQL.Tx.Squeal.Internal
  ( -- * Disclaimer
    -- $disclaimer

    -- ** Internals
    module Database.PostgreSQL.Tx.Squeal.Internal
  ) where

import Data.Kind (Constraint)
import Database.PostgreSQL.Tx (TxEnvs, TxM, askTxEnv)
import Database.PostgreSQL.Tx.Squeal.Internal.Reexport
import Database.PostgreSQL.Tx.Unsafe (unsafeRunIOInTxM, unsafeRunTxM, unsafeLookupTxEnvIO)
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Squeal.PostgreSQL as Squeal

-- | Runtime environment needed to run @squeal-postgresql@ via @postgresql-tx@.
--
-- @since 0.2.0.0
type SquealEnv (db :: Squeal.SchemasType) r =
  (TxEnvs '[SquealSchemas db, SquealConnection] r) :: Constraint

-- | Monad type alias for running @squeal-postgresql@ via @postgresql-tx@.
--
-- @since 0.2.0.0
type SquealM db a = forall r. (SquealEnv db r) => TxM r a

-- | Used in the 'SquealEnv' to specify the applicable schemas in which
-- a 'TxM' can be run.
--
-- @since 0.2.0.0
data SquealSchemas (db :: Squeal.SchemasType) = SquealSchemas

-- | Used in the 'SquealEnv' to specify the 'LibPQ.Connection' to use.
-- Should produce the same 'LibPQ.Connection' if called multiple times
-- in the same transaction. Usually you will want to use 'mkSquealConnection'
-- to get one.
--
-- @since 0.2.0.0
data SquealConnection =
  UnsafeSquealConnection
    { unsafeWithLibPQConnection :: forall a. (LibPQ.Connection -> IO a) -> IO a
    }

-- | Construct a 'SquealConnection' from a 'LibPQ.Connection'.
--
-- @since 0.2.0.0
mkSquealConnection :: LibPQ.Connection -> SquealConnection
mkSquealConnection conn = UnsafeSquealConnection ($ conn)

unsafeSquealIOTxM
  :: forall db r a. (SquealEnv db r)
  => PQ db db IO a -> TxM r a
unsafeSquealIOTxM (Squeal.PQ f) = do
  UnsafeSquealConnection { unsafeWithLibPQConnection } <- askTxEnv
  unsafeRunIOInTxM $ unsafeWithLibPQConnection \conn -> do
    Squeal.K a <- f (Squeal.K conn)
    pure a

unsafeSquealIOTxM1
  :: forall db r x1 a. (SquealEnv db r)
  => (x1 -> PQ db db IO a)
  -> x1 -> TxM r a
unsafeSquealIOTxM1 f x1 = unsafeSquealIOTxM $ f x1

unsafeSquealIOTxM2
  :: forall db r x1 x2 a. (SquealEnv db r)
  => (x1 -> x2 -> PQ db db IO a)
  -> x1 -> x2 -> TxM r a
unsafeSquealIOTxM2 f x1 x2 = unsafeSquealIOTxM $ f x1 x2

unsafeSquealIOTxM3
  :: forall db r x1 x2 x3 a. (SquealEnv db r)
  => (x1 -> x2 -> x3 -> PQ db db IO a)
  -> x1 -> x2 -> x3 -> TxM r a
unsafeSquealIOTxM3 f x1 x2 x3 = unsafeSquealIOTxM $ f x1 x2 x3

unsafeRunSquealTransaction
  :: forall db r a. (SquealEnv db r)
  => (PQ db db IO a -> PQ db db IO a)
  -> r
  -> TxM r a
  -> IO a
unsafeRunSquealTransaction f r x = do
  UnsafeSquealConnection { unsafeWithLibPQConnection } <- unsafeLookupTxEnvIO r
  unsafeWithLibPQConnection \conn -> do
    flip Squeal.evalPQ (Squeal.K conn)
      $ f
      $ PQ \_ -> Squeal.K <$> unsafeRunTxM r x

-- $disclaimer
--
-- Changes to this module will not be reflected in the library's version
-- updates.
