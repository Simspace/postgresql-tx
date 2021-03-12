{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Database.PostgreSQL.Tx.LibPQ.Connection
  ( LibPQConnection(unsafeGetLibPQConnection)
  , usingLibPQConnection
  ) where

import Database.PostgreSQL.LibPQ (Connection)
import Database.PostgreSQL.Tx.HEnv (HEnv)
import qualified Database.PostgreSQL.Tx.HEnv as HEnv

newtype LibPQConnection = UnsafeLibPQConnection
  { unsafeGetLibPQConnection :: Connection
  }

usingLibPQConnection
  :: (LibPQConnection `HEnv.NotElem` xs)
  => Connection -> HEnv xs -> (HEnv (LibPQConnection ': xs) -> IO a) -> IO a
usingLibPQConnection conn henv action = do
  let libPQConn = UnsafeLibPQConnection conn
  let henv' = libPQConn `HEnv.Cons` henv
  action henv'
