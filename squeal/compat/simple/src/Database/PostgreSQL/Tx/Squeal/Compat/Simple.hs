{-# LANGUAGE BlockArguments #-}
module Database.PostgreSQL.Tx.Squeal.Compat.Simple
  ( withSquealConnection
  ) where

import Database.PostgreSQL.Tx.Squeal.Internal (SquealConnection(UnsafeSquealConnection))
import qualified Control.Concurrent as Concurrent
import qualified Database.PostgreSQL.Simple as Simple
import qualified Database.PostgreSQL.Simple.Internal as Simple.Internal

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
withSquealConnection conn f = do
  f $ UnsafeSquealConnection
    $ Concurrent.readMVar
    $ Simple.Internal.connectionHandle conn
