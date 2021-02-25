{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
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

import Control.Exception (Exception(fromException))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Kind (Constraint)
import Database.PostgreSQL.Tx (TxEnv, TxException, TxM, askTxEnv, mapExceptionTx)
import Database.PostgreSQL.Tx.LibPQ.Connection
import Database.PostgreSQL.Tx.Squeal.Internal.Reexport
import Database.PostgreSQL.Tx.Unsafe (unsafeLookupTxEnvIO, unsafeMkTxException, unsafeRunIOInTxM, unsafeRunTxM)
import GHC.TypeLits (ErrorMessage(Text), TypeError)
import UnliftIO (MonadUnliftIO)
import qualified Data.ByteString.Char8 as Char8
import qualified Squeal.PostgreSQL as Squeal
import qualified UnliftIO

-- | Runtime environment needed to run @squeal-postgresql@ via @postgresql-tx@.
--
-- @since 0.2.0.0
type SquealEnv r =
  (TxEnv LibPQConnection r) :: Constraint

-- | Monad type alias for running @squeal-postgresql@ via @postgresql-tx@.
--
-- @since 0.2.0.0
type SquealM a = forall r. (SquealEnv r) => TxM r a

-- | Alias for 'SquealTxM'' but has the 'SquealEnv' constraint applied to @r@.
--
-- @since 0.2.0.0
type SquealTxM (db :: SchemasType) a =
  forall r. (SquealEnv r) => SquealTxM' db r a

-- | A newtype wrapper around 'TxM' which includes the @squeal@ 'SchemasType'
-- parameter @db@. This is used only as type information.
-- You can easily convert 'TxM' to and from 'SquealTxM'' by using the
-- 'SquealTxM'' constructor and 'fromSquealTxM' function, respectively.
--
-- In practice, you will likely prefer to use the 'SquealTxM' type alias
-- as it includes the 'SquealEnv' constraint on @r@.
--
-- @since 0.2.0.0
newtype SquealTxM' (db :: SchemasType) r a =
  SquealTxM
    { -- | Convert a 'SquealTxM'' to a 'TxM'.
      --
      -- @since 0.2.0.0
      fromSquealTxM :: TxM r a
    }
  deriving newtype (Functor, Applicative, Monad)

-- | The 'SquealTxM'' monad discourages performing arbitrary 'IO' within a
-- transaction, so this instance generates a type error when client code tries
-- to call 'liftIO'.
--
-- Note that we specialize this instance for 'SquealTxM'' rather than derive it
-- via newtype so we can provide a better error message.
--
-- @since 0.2.0.0
instance
  ( TypeError
      ('Text "MonadIO is banned in SquealTxM'; use 'SquealTxM . unsafeRunIOInTxM' if you are sure this is safe IO")
  ) => MonadIO (SquealTxM' db r)
  where
  liftIO = undefined

fromSquealException :: SquealException -> TxException
fromSquealException =
  unsafeMkTxException \case
    SQLException SQLState { sqlStateCode } -> Just $ Char8.unpack sqlStateCode
    _ -> Nothing

unsafeSquealIOTxM
  :: PQ db db IO a
  -> SquealTxM db a
unsafeSquealIOTxM (PQ f) = SquealTxM $ mapExceptionTx (Just . fromSquealException) do
  conn <- unsafeGetLibPQConnection <$> askTxEnv
  unsafeRunIOInTxM do
    K a <- f (K conn)
    pure a

unsafeSquealIOTxM1
  :: (x1 -> PQ db db IO a)
  -> x1 -> SquealTxM db a
unsafeSquealIOTxM1 f x1 = unsafeSquealIOTxM $ f x1

unsafeSquealIOTxM2
  :: (x1 -> x2 -> PQ db db IO a)
  -> x1 -> x2 -> SquealTxM db a
unsafeSquealIOTxM2 f x1 x2 = unsafeSquealIOTxM $ f x1 x2

unsafeSquealIOTxM3
  :: (x1 -> x2 -> x3 -> PQ db db IO a)
  -> x1 -> x2 -> x3 -> SquealTxM db a
unsafeSquealIOTxM3 f x1 x2 x3 = unsafeSquealIOTxM $ f x1 x2 x3

unsafeRunSquealTransaction
  :: forall r a. (SquealEnv r)
  => (forall db. PQ db db IO a -> PQ db db IO a)
  -> r
  -> TxM r a
  -> IO a
unsafeRunSquealTransaction f r x = do
  conn <- unsafeGetLibPQConnection <$> unsafeLookupTxEnvIO r
  flip evalPQ (K conn)
    $ f
    $ PQ \_ -> K <$> unsafeRunTxM r x

-- | A variant of 'transactionallyRetry' which takes a predicate
-- for determining when to retry instead of only doing so on
-- @serialization_failure@.
transactionallyRetry'
  :: (MonadUnliftIO m, MonadPQ db m, Exception e)
  => TransactionMode
  -> (e -> Bool)
  -> m a
  -> m a
transactionallyRetry' mode shouldRetry action = UnliftIO.mask $ \restore ->
  loop . UnliftIO.try $ do
    x <- restore action
    Squeal.manipulate_ commit
    return x
  where
  loop attempt = do
    Squeal.manipulate_ $ begin mode
    attempt >>= \case
      Right a -> return a
      Left e -> do
        Squeal.manipulate_ rollback
        if any shouldRetry (fromException e) then
          loop attempt
        else
          UnliftIO.throwIO e

-- $disclaimer
--
-- Changes to this module will not be reflected in the library's version
-- updates.
