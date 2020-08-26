{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Database.PostgreSQL.Tx.MonadLogger.Unsafe where

import Control.Monad.Logger (LoggingT(runLoggingT))
import Data.Proxy (Proxy(Proxy))
import Database.PostgreSQL.Tx (TxEnv(withTxEnv), TxM)
import Database.PostgreSQL.Tx.MonadLogger (Logger)
import Database.PostgreSQL.Tx.Unsafe (unsafeRunIOInTxM)

-- | Promote a 'LoggingT' over 'IO' to 'TxM'.
-- Use this function with care - arbitrary 'IO' should only be run
-- within a transaction when truly necessary.
--
-- @since 0.2.0.0
unsafeRunLoggerIOInTxM :: (TxEnv r Logger) => LoggingT IO a -> TxM r a
unsafeRunLoggerIOInTxM x =
  withTxEnv (Proxy @Logger) \logger -> do
    unsafeRunIOInTxM $ runLoggingT x logger
