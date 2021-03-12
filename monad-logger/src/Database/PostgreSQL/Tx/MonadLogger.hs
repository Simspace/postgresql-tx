{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.PostgreSQL.Tx.MonadLogger where

import Control.Monad.Logger (LoggingT(LoggingT), MonadLogger(monadLoggerLog), Loc, LogLevel, LogSource, LogStr, toLogStr)
import Database.PostgreSQL.Tx (TxEnv, TxM, askTxEnv)
import Database.PostgreSQL.Tx.HEnv (HEnv)
import Database.PostgreSQL.Tx.Unsafe (unsafeRunIOInTxM)
import qualified Database.PostgreSQL.Tx.HEnv as HEnv

-- | A logging function compatible with @monad-logger@.
-- Exists as a @newtype@ wrapper to simplify type errors.
--
-- @since 0.4.0.0
newtype Logger = Logger { getLogger :: Loc -> LogSource -> LogLevel -> LogStr -> IO () }

-- | Adds the specified logger function to the given 'HEnv'
-- as a 'Logger'.
--
-- @since 0.4.0.0
usingLogger
  :: (Logger `HEnv.NotElem` xs)
  => (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> HEnv xs
  -> (HEnv (Logger ': xs) -> IO a)
  -> IO a
usingLogger logger henv action = do
  action (Logger logger `HEnv.Cons` henv)

-- | Adds the specified 'LoggingT' function to the given 'HEnv'
-- as a 'Logger'.
--
-- @since 0.4.0.0
usingLoggingT
  :: (Logger `HEnv.NotElem` xs)
  => (LoggingT IO () -> IO ())
  -> HEnv xs
  -> (HEnv (Logger ': xs) -> IO a)
  -> IO a
usingLoggingT loggingT = usingLogger (toLogger loggingT)
  where
  toLogger f loc src lvl msg =
    f $ LoggingT \logger -> logger loc src lvl msg

-- | Orphan instance for running @monad-logger@ functions in 'TxM'.
--
-- @since 0.2.0.0
instance (TxEnv Logger r) => MonadLogger (TxM r) where
  monadLoggerLog loc src lvl msg = do
    Logger logger <- askTxEnv
    unsafeRunIOInTxM $ logger loc src lvl (toLogStr msg)
