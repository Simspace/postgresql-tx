{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.Tx.Squeal.Internal
  ( -- * Disclaimer
    -- $disclaimer

    -- ** Internals
    module Database.PostgreSQL.Tx.Squeal.Internal
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Kind (Constraint)
import Database.PostgreSQL.Tx (TxEnv, TxM, askTxEnv)
import Database.PostgreSQL.Tx.Squeal.Internal.Reexport
import Database.PostgreSQL.Tx.Unsafe (unsafeLookupTxEnvIO, unsafeRunIOInTxM, unsafeRunTxM)
import GHC.TypeLits (ErrorMessage(Text), TypeError)
import qualified Database.PostgreSQL.LibPQ as LibPQ

-- | Runtime environment needed to run @squeal-postgresql@ via @postgresql-tx@.
--
-- @since 0.2.0.0
type SquealEnv r =
  (TxEnv SquealConnection r) :: Constraint

-- | Monad type alias for running @squeal-postgresql@ via @postgresql-tx@.
--
-- @since 0.2.0.0
type SquealM a = forall r. (SquealEnv r) => TxM r a

-- | Alias for 'SquealTxM' but has the 'SquealEnv' constraint applied to @r@
-- and uses @db@ for both @db0@ and @db1@ since this is the common case.
--
-- @since 0.2.0.0
type DefaultSquealTxM (db :: SchemasType) a =
  forall r. (SquealEnv r) => SquealTxM db db r a

-- | A newtype wrapper around 'TxM' which includes the @squeal@ 'SchemasType'
-- parameters @db0@ and @db1@. These are used only as type information.
-- You can easily convert 'TxM' to and from 'SquealTxM' by using the
-- 'SquealTxM' constructor and 'fromSquealTxM' function, respectively.
--
-- @since 0.2.0.0
newtype SquealTxM (db0 :: SchemasType) (db1 :: SchemasType) r a =
  SquealTxM
    { -- | Convert a 'SquealTxM' to a 'TxM'.
      --
      -- @since 0.2.0.0
      fromSquealTxM :: TxM r a
    }
  deriving newtype (Functor, Applicative, Monad)

-- | The 'SquealTxM' monad discourages performing arbitrary 'IO' within a
-- transaction, so this instance generates a type error when client code tries
-- to call 'liftIO'.
--
-- Note that specialize this instance for 'SquealTxM' rather than derive it
-- via newtype so we can provide a better error message.
--
-- @since 0.2.0.0
instance
  ( TypeError
      ('Text "MonadIO is banned in SquealTxM; use 'SquealTxM . unsafeRunIOInTxM' if you are sure this is safe IO")
  ) => MonadIO (SquealTxM db0 db1 r)
  where
  liftIO = undefined

-- | Used in the 'SquealEnv' to specify the 'LibPQ.Connection' to use.
-- Should produce the same 'LibPQ.Connection' if called multiple times
-- in the same transaction. Usually you will want to use 'mkSquealConnection'
-- to get one.
--
-- @since 0.2.0.0
newtype SquealConnection =
  UnsafeSquealConnection
    { unsafeGetLibPQConnection :: IO LibPQ.Connection
    }

-- | Construct a 'SquealConnection' from a 'LibPQ.Connection'.
--
-- @since 0.2.0.0
mkSquealConnection :: LibPQ.Connection -> SquealConnection
mkSquealConnection conn = UnsafeSquealConnection (pure conn)

unsafeSquealIOTxM
  :: (SquealEnv r)
  => PQ db db IO a
  -> SquealTxM db db r a
unsafeSquealIOTxM (PQ f) = SquealTxM do
  UnsafeSquealConnection { unsafeGetLibPQConnection } <- askTxEnv
  unsafeRunIOInTxM do
    conn <- unsafeGetLibPQConnection
    K a <- f (K conn)
    pure a

unsafeSquealIOTxM1
  :: (SquealEnv r)
  => (x1 -> PQ db db IO a)
  -> x1 -> SquealTxM db db r a
unsafeSquealIOTxM1 f x1 = unsafeSquealIOTxM $ f x1

unsafeSquealIOTxM2
  :: (SquealEnv r)
  => (x1 -> x2 -> PQ db db IO a)
  -> x1 -> x2 -> SquealTxM db db r a
unsafeSquealIOTxM2 f x1 x2 = unsafeSquealIOTxM $ f x1 x2

unsafeSquealIOTxM3
  :: (SquealEnv r)
  => (x1 -> x2 -> x3 -> PQ db db IO a)
  -> x1 -> x2 -> x3 -> SquealTxM db db r a
unsafeSquealIOTxM3 f x1 x2 x3 = unsafeSquealIOTxM $ f x1 x2 x3

unsafeRunSquealTransaction
  :: forall r a. (SquealEnv r)
  => (forall db. PQ db db IO a -> PQ db db IO a)
  -> r
  -> TxM r a
  -> IO a
unsafeRunSquealTransaction f r x = do
  UnsafeSquealConnection { unsafeGetLibPQConnection } <- unsafeLookupTxEnvIO r
  conn <- unsafeGetLibPQConnection
  flip evalPQ (K conn)
    $ f
    $ PQ \_ -> K <$> unsafeRunTxM r x

-- $disclaimer
--
-- Changes to this module will not be reflected in the library's version
-- updates.
