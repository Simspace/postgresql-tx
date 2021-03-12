{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Database.PostgreSQL.Tx.Simple.Connection
  ( PgSimpleConnection(unsafeGetPgSimpleConnection)
  , usingPgSimpleConnection
  , module Database.PostgreSQL.Tx.LibPQ.Connection
  ) where

import Control.Concurrent (readMVar)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Tx.HEnv (HEnv)
import Database.PostgreSQL.Tx.LibPQ.Connection
import qualified Database.PostgreSQL.Simple.Internal as Simple.Internal
import qualified Database.PostgreSQL.Tx.HEnv as HEnv

newtype PgSimpleConnection = UnsafePgSimpleConnection
  { unsafeGetPgSimpleConnection :: Connection
  }

usingPgSimpleConnection
  :: ( PgSimpleConnection `HEnv.NotElem` xs
     , LibPQConnection `HEnv.NotElem` xs
     )
  => Connection
  -> HEnv xs
  -> (HEnv (PgSimpleConnection ': LibPQConnection ': xs) -> IO a)
  -> IO a
usingPgSimpleConnection conn henv0 action = do
  pq <- readMVar $ Simple.Internal.connectionHandle conn
  usingLibPQConnection pq henv0 \henv1 -> do
    let pgSimpleConn = UnsafePgSimpleConnection conn
    let henv2 = pgSimpleConn `HEnv.Cons` henv1
    action henv2
