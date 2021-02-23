{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Database.PostgreSQL.Tx.Squeal.Compat.Simple.Internal where

import Database.PostgreSQL.Tx.HEnv (HEnv)
import Database.PostgreSQL.Tx.Squeal.Internal (SquealConnection(UnsafeSquealConnection))
import qualified Control.Concurrent as Concurrent
import qualified Database.PostgreSQL.Simple as Simple
import qualified Database.PostgreSQL.Simple.Internal as Simple.Internal
import qualified Database.PostgreSQL.Tx.HEnv as HEnv

usingSquealConnection
  :: (HEnv.UniqueElem Simple.Connection xs)
  => HEnv xs -> (HEnv (SquealConnection ': xs) -> IO a) -> IO a
usingSquealConnection henv action = do
  let simpleConn = HEnv.select henv
  withSquealConnection simpleConn \squealConn -> do
    action $ squealConn `HEnv.Cons` henv

-- | Used in the 'Database.PostgreSQL.Tx.Squeal.Internal.SquealEnv' to specify
-- the 'Database.PostgreSQL.Tx.Squeal.Internal.SquealConnection' from a
-- @postgresql-simple@ 'Simple.Connection'.
--
-- 'Database.PostgreSQL.Tx.Squeal.Internal.mkSquealConnection' should be
-- preferred over this function if you are only working with
-- @postgresql-libpq@ connections.
--
-- @since 0.1.0.0
withSquealConnection :: Simple.Connection -> (SquealConnection -> IO a) -> IO a
withSquealConnection conn action = do
  action
    $ UnsafeSquealConnection
    $ Concurrent.readMVar
    $ Simple.Internal.connectionHandle conn
