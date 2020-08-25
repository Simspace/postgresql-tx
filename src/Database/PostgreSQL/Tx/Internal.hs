{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.Tx.Internal
  ( -- * Disclaimer
    -- $disclaimer

    -- ** Internals
    module Database.PostgreSQL.Tx.Internal
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Reader (ReaderT(ReaderT, runReaderT), ask)
import Data.Kind (Constraint)
import GHC.TypeLits (ErrorMessage(Text), TypeError)

-- | The transaction monad. Unifies all database integrations, regardless of
-- library, into a single monad. The @r@ type parameter represents the reader
-- environment needed for applicable database libraries. For example,
-- @postgresql-simple@ needs a @Connection@ to run its functions, so
-- its interface will require that we can obtain a @Connection@ from the @r@
-- using the 'TxEnv' type class.
--
-- @since 0.2.0.0
newtype TxM r a = UnsafeTxM
  { -- | Convert a 'TxM' action to raw 'ReaderT' over 'IO'. This is provided only to give
    -- adaptor libraries access to the underlying 'IO' that 'TxM' wraps.
    --
    -- @since 0.2.0.0
    unsafeUnTxM :: ReaderT r IO a
  } deriving newtype (Functor, Applicative, Monad)

-- | Run an 'IO' action in 'TxM'. Use this function with care - arbitrary 'IO'
-- should only be run within a transaction when truly necessary.
--
-- @since 0.2.0.0
unsafeRunIOInTxM :: IO a -> TxM r a
unsafeRunIOInTxM = UnsafeTxM . liftIO

-- | Construct a 'TxM' using a reader function. Use this function with care -
-- arbitrary 'IO' should only be run within a transaction when truly necessary.
--
-- @since 0.2.0.0
unsafeMkTxM :: (r -> IO a) -> TxM r a
unsafeMkTxM = UnsafeTxM . ReaderT

-- | Similar to 'unsafeMkTxM' but allows for constructing a 'TxM' with a
-- reader function using a specific value from the environment.
-- Use this function with care - arbitrary 'IO' should only be run
-- within a transaction when truly necessary.
--
-- @since 0.2.0.0
unsafeMksTxM :: (TxEnv r a) => proxy a -> (a -> IO b) -> TxM r b
unsafeMksTxM proxy f =
  unsafeMkTxM \r -> unsafeRunTxM r do
    withTxEnv proxy \a -> do
      unsafeRunIOInTxM $ f a

-- | The 'TxM' monad discourages performing arbitrary 'IO' within a
-- transaction, so this instance generates a type error when client code tries
-- to call 'liftIO'.
--
-- @since 0.1.0.0
instance
  ( TypeError
      ('Text "MonadIO is banned in TxM; use 'unsafeRunIOInTxM' if you are sure this is safe IO")
  ) => MonadIO (TxM r)
  where
  liftIO = undefined

-- | Run a 'TxM' to 'IO' given the database runtime environment @r@.
-- Use of this function outside of test suites should be rare.
--
-- @since 0.2.0.0
unsafeRunTxM :: r -> TxM r a -> IO a
unsafeRunTxM r x = runReaderT (unsafeUnTxM x) r

-- | Run a 'TxM' action in 'IO' via the provided runner function. Use this
-- function with care - arbitrary 'IO' should only be run within a transaction
-- when truly necessary.
--
-- @since 0.2.0.0
unsafeWithRunInIOTxM :: ((forall a. TxM r a -> IO a) -> IO b) -> TxM r b
unsafeWithRunInIOTxM inner = unsafeMkTxM \r -> inner (unsafeRunTxM r)

-- | A type class for specifying how to acquire an environment value
-- to be used for running an implementation of a database library.
-- For example, your database library will likely require some sort of
-- connection value to discharge its effects; in this case, you'd want to
-- define an instance of @TxEnv MyDBEnv Connection@ and use @TxM MyDBEnv@
-- as your monad for executing transactions.
--
-- When implementing instances for this type class, you are encouraged
-- to use one of the supplied helper implementations: 'withTxEnv'Selecting',
-- 'withTxEnv'Resource', 'withTxEnv'Singleton'.
--
-- Note that implementations should take care and ensure that multiple
-- instances are compatible with one another. For example, let's say you
-- have instances for both @TxEnv E PgSimple.Connection@ and
-- @TxEnv E LibPQ.Connection@; if both of these implementations are grabbing
-- connections from a pool, you will end up with each of those database
-- libraries using different connections, and thus, would be running in
-- separate transactions!
--
-- @since 0.2.0.0
class TxEnv r a where

  -- | Acquire a value @a@ via the reader environment @r@ which assists in
  -- running a 'TxM' in a transaction.
  --
  -- @since 0.2.0.0
  withTxEnv :: proxy a -> (a -> TxM r x) -> TxM r x

-- | Derive an implementation of 'withTxEnv' using a function, most likely
-- a field selector.
--
-- @since 0.2.0.0
withTxEnv'Selecting
  :: (TxEnv r a)
  => (r -> a)
  -> proxy a -> (a -> TxM r x) -> TxM r x
withTxEnv'Selecting selector _ f = do
  r <- UnsafeTxM ask
  f (selector r)

-- | Derive an implementation of 'withTxEnv' using a resource acquiring
-- function.
--
-- @since 0.2.0.0
withTxEnv'Resource
  :: (TxEnv r a)
  => (r -> (a -> IO x) -> IO x)
  -> proxy a -> (a -> TxM r x) -> TxM r x
withTxEnv'Resource acquire _ f = do
  r <- UnsafeTxM ask
  unsafeWithRunInIOTxM \run -> do
    acquire r \a -> run (f a)

-- | Derive an implementation of 'withTxEnv' using a singleton value.
--
-- @since 0.2.0.0
withTxEnv'Singleton
  :: a
  -> proxy a -> (a -> TxM r x) -> TxM r x
withTxEnv'Singleton a _ f = f a

-- | Analogous to 'withTxEnv' but can be run in 'IO' instead of 'TxM'.
--
-- @since 0.2.0.0
unsafeWithTxEnvIO :: (TxEnv r a) => proxy a -> r -> (a -> IO x) -> IO x
unsafeWithTxEnvIO proxy r f = do
  unsafeRunTxM r do
    withTxEnv proxy \a ->
      unsafeRunIOInTxM $ f a

-- | Type family which allows for specifying several 'TxEnv' constraints as
-- a type-level list.
--
-- @since 0.2.0.0
type family TxEnvs r (xs :: [*]) :: Constraint where
  TxEnvs r '[] = ()
  TxEnvs r (x ': xs) = (TxEnv r x, TxEnvs r xs)

-- $disclaimer
--
-- Changes to this module will not be reflected in the library's version
-- updates.
