{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Database.PostgreSQL.Tx.Squeal.Internal
  ( -- * Disclaimer
    -- $disclaimer

    -- ** Internals
    module Database.PostgreSQL.Tx.Squeal.Internal
  ) where

import Data.Kind (Constraint)
import Database.PostgreSQL.Tx (TxEnvs, TxM, withTxEnv)
import Database.PostgreSQL.Tx.Squeal.Internal.Reexport
import Database.PostgreSQL.Tx.Unsafe (unsafeRunIOInTxM, unsafeRunTxM, unsafeWithTxEnvIO)
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Squeal.PostgreSQL as Squeal

-- | Runtime environment needed to run @squeal-postgresql@ via @postgresql-tx@.
--
-- @since 0.2.0.0
type SquealEnv (db :: Squeal.SchemasType) r =
  (TxEnvs r '[SquealSchemas db, LibPQ.Connection]) :: Constraint

-- | Monad type alias for running @squeal-postgresql@ via @postgresql-tx@.
--
-- @since 0.2.0.0
type SquealM db a = forall r. (SquealEnv db r) => TxM r a

-- | Used in the 'SquealEnv' to specify the applicable schemas in which
-- a 'TxM' can be run.
--
-- @since 0.2.0.0
data SquealSchemas (db :: Squeal.SchemasType) =
  SquealSchemas

unsafeSquealIOTxM
  :: forall db r a. (SquealEnv db r)
  => PQ db db IO a -> TxM r a
unsafeSquealIOTxM (Squeal.PQ f) = do
  withTxEnv \conn ->
    unsafeRunIOInTxM do
      Squeal.K a <- f (Squeal.K conn)
      pure a

unsafeSquealIOTx1
  :: forall db r x1 a. (SquealEnv db r)
  => (x1 -> PQ db db IO a)
  -> x1 -> TxM r a
unsafeSquealIOTx1 f x1 = unsafeSquealIOTxM $ f x1

unsafeSquealIOTx2
  :: forall db r x1 x2 a. (SquealEnv db r)
  => (x1 -> x2 -> PQ db db IO a)
  -> x1 -> x2 -> TxM r a
unsafeSquealIOTx2 f x1 x2 = unsafeSquealIOTxM $ f x1 x2

unsafeSquealIOTx3
  :: forall db r x1 x2 x3 a. (SquealEnv db r)
  => (x1 -> x2 -> x3 -> PQ db db IO a)
  -> x1 -> x2 -> x3 -> TxM r a
unsafeSquealIOTx3 f x1 x2 x3 = unsafeSquealIOTxM $ f x1 x2 x3

unsafeRunSquealTransaction
  :: forall db r a. (SquealEnv db r)
  => (PQ db db IO a -> PQ db db IO a)
  -> r
  -> TxM r a
  -> IO a
unsafeRunSquealTransaction f r x = do
  unsafeWithTxEnvIO r \conn -> do
    flip Squeal.evalPQ (Squeal.K conn)
      $ f
      $ PQ \_ -> Squeal.K <$> unsafeRunTxM r x

-- $disclaimer
--
-- Changes to this module will not be reflected in the library's version
-- updates.
