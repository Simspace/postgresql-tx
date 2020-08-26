{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.PostgreSQL.Tx.MonadLogger where

import Control.Monad.Logger (MonadLogger(monadLoggerLog), Loc, LogLevel, LogSource, LogStr, toLogStr)
import Data.Proxy (Proxy(Proxy))
import Database.PostgreSQL.Tx (TxEnv(withTxEnv), TxM)
import Database.PostgreSQL.Tx.Unsafe (unsafeRunIOInTxM)

-- | A logging function compatible with @monad-logger@.
--
-- @since 0.1.0.0
type Logger = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

-- | Orphan instance for running @monad-logger@ functions in 'TxM'.
--
-- @since 0.2.0.0
instance (TxEnv r Logger) => MonadLogger (TxM r) where
  monadLoggerLog loc src lvl msg = do
    withTxEnv \logger -> do
      unsafeRunIOInTxM $ logger loc src lvl (toLogStr msg)
