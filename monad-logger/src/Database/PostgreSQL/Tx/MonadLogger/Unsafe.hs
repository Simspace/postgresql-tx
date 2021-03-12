{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Database.PostgreSQL.Tx.MonadLogger.Unsafe where

import Control.Monad.Logger (LoggingT(runLoggingT))
import Database.PostgreSQL.Tx (TxEnv, TxM, askTxEnv)
import Database.PostgreSQL.Tx.MonadLogger (Logger(Logger))
import Database.PostgreSQL.Tx.Unsafe (unsafeRunIOInTxM)

-- | Promote a 'LoggingT' over 'IO' to 'TxM'.
-- Use this function with care - arbitrary 'IO' should only be run
-- within a transaction when truly necessary.
--
-- @since 0.2.0.0
unsafeRunLoggerIOInTxM :: (TxEnv Logger r) => LoggingT IO a -> TxM r a
unsafeRunLoggerIOInTxM x = do
  Logger logger <- askTxEnv
  unsafeRunIOInTxM $ runLoggingT x logger
