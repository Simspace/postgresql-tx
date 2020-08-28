{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Database.PostgreSQL.Tx.Simple.Internal
  ( -- * Disclaimer
    -- $disclaimer

    -- ** Internals
    module Database.PostgreSQL.Tx.Simple.Internal
  ) where

import Data.Kind (Constraint)
import Database.PostgreSQL.Tx (TxEnv, TxM, askTxEnv)
import Database.PostgreSQL.Tx.Unsafe (unsafeMksTxM, unsafeRunIOInTxM, unsafeRunTxM)
import qualified Database.PostgreSQL.Simple as Simple

-- | Runtime environment needed to run @postgresql-simple@ via @postgresql-tx@.
--
-- @since 0.2.0.0
type PgSimpleEnv r = (TxEnv Simple.Connection r) :: Constraint

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
    conn <- askTxEnv
    unsafeRunIOInTxM $ f conn (unsafeRunTxM r x)

unsafeFromPgSimple1
  :: (Simple.Connection -> a1 -> IO x)
  -> a1 -> PgSimpleM x
unsafeFromPgSimple1 f a1 = unsafeMksTxM \c -> f c a1

unsafeFromPgSimple2
  :: (Simple.Connection -> a1 -> a2 -> IO x)
  -> a1 -> a2 -> PgSimpleM x
unsafeFromPgSimple2 f a1 a2 = unsafeMksTxM \c -> f c a1 a2

-- $disclaimer
--
-- Changes to this module will not be reflected in the library's version
-- updates.
