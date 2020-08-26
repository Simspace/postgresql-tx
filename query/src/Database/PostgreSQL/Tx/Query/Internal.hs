{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Database.PostgreSQL.Tx.Query.Internal
  ( -- * Disclaimer
    -- $disclaimer

    -- ** Internals
    module Database.PostgreSQL.Tx.Query.Internal
  ) where

import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger(monadLoggerLog), toLogStr)
import Control.Monad.Reader (ReaderT(ReaderT))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Kind (Constraint)
import Data.Proxy (Proxy(Proxy))
import Database.PostgreSQL.Tx (TxEnvs, TxM, withTxEnv)
import Database.PostgreSQL.Tx.Query.Internal.Reexport
import Database.PostgreSQL.Tx.Unsafe (unsafeMkTxM, unsafeRunIOInTxM, unsafeRunTxM, unsafeUnTxM)
import GHC.Stack (HasCallStack)
import qualified Database.PostgreSQL.Simple as Simple
import qualified Database.PostgreSQL.Tx.MonadLogger

-- | Runtime environment needed to run @postgresql-query@ via @postgresql-tx@.
--
-- @since 0.2.0.0
type PgQueryEnv r = (TxEnvs r '[Simple.Connection, Logger]) :: Constraint

-- | Monad type alias for running @postgresql-query@ via @postgresql-tx@.
--
-- @since 0.2.0.0
type PgQueryM a = forall r. (PgQueryEnv r) => TxM r a

-- | Re-export of 'Database.PostgreSQL.Tx.MonadLogger.Logger'.
--
-- @since 0.1.0.0
type Logger = Database.PostgreSQL.Tx.MonadLogger.Logger

-- | Analogous to 'TxM' but allows for 'IO'. Useful so we can have
-- instances which are required to run @postgresql-query@ functions.
-- See 'unsafeToPgQueryIO' and 'unsafeFromPgQueryIO' for converting to/from
-- 'TxM'.
newtype UnsafePgQueryIO r a = UnsafePgQueryIO (ReaderT r IO a)
  deriving newtype
    ( Functor, Applicative, Monad, MonadIO
    , MonadBase IO, MonadBaseControl IO, TransactionSafe
    )

instance (PgQueryEnv r) => HasPostgres (UnsafePgQueryIO r) where
  withPGConnection f = do
    unsafeToPgQueryIO do
      withTxEnv (Proxy @Simple.Connection) \conn -> do
        unsafeFromPgQueryIO $ f conn

instance (PgQueryEnv r) => MonadLogger (UnsafePgQueryIO r) where
  monadLoggerLog loc src lvl msg = do
    unsafeToPgQueryIO do
      withTxEnv (Proxy @Logger) \logger -> do
        unsafeRunIOInTxM $ logger loc src lvl (toLogStr msg)

unsafeToPgQueryIO :: (HasCallStack) => TxM r a -> UnsafePgQueryIO r a
unsafeToPgQueryIO x = UnsafePgQueryIO $ unsafeUnTxM x

unsafeFromPgQueryIO :: (HasCallStack) => UnsafePgQueryIO r a -> TxM r a
unsafeFromPgQueryIO (UnsafePgQueryIO (ReaderT f)) = unsafeMkTxM f

unsafeRunPgQueryTransaction
  :: (PgQueryEnv r, HasCallStack)
  => (HasCallStack => UnsafePgQueryIO r a -> UnsafePgQueryIO r a)
  -> r
  -> TxM r a
  -> IO a
unsafeRunPgQueryTransaction f r x =
  unsafeRunTxM r
    $ unsafeFromPgQueryIO
    $ f
    $ unsafeToPgQueryIO x

-- $disclaimer
--
-- Changes to this module will not be reflected in the library's version
-- updates.
