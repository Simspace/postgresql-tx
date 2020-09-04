{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Example.PgSimple.Internal.Queries where

import Database.PostgreSQL.Tx (TxM)
import qualified Control.Exception as Exception
import qualified Database.PostgreSQL.Simple as PG.Simple
import qualified Database.PostgreSQL.Tx.Simple as Tx
import qualified Example.PgSimple.Internal.DB as DB

new :: (Tx.PgSimpleEnv r) => IO (DB.Handle (TxM r))
new =
  pure DB.Handle
    { DB.insertMessage
    , DB.fetchMessage

    , DB.close = mempty
    }

withHandle :: (Tx.PgSimpleEnv r) => ((DB.Handle (TxM r)) -> IO a) -> IO a
withHandle = Exception.bracket new DB.close

insertMessage :: String -> Tx.PgSimpleM Int
insertMessage s = do
  Tx.query
    "insert into foo(message) values (?) returning id"
    (PG.Simple.Only s)
    >>= \case
      [PG.Simple.Only k] -> pure k
      rows -> error $ "Expected exactly 1 row, got " <> show (length rows)

fetchMessage :: Int -> Tx.PgSimpleM (Maybe String)
fetchMessage k = do
  Tx.query
    "select message from foo where id = ?"
    (PG.Simple.Only k)
    >>= \case
      [] -> pure Nothing
      [PG.Simple.Only s] -> pure $ Just s
      rows -> error $ "Expected 0 or 1 rows, got " <> show (length rows)
