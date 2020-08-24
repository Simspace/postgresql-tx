{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
module Example.PgQuery.Internal.Queries where

import qualified Database.PostgreSQL.Tx.Query as Tx
import qualified Control.Exception as Exception
import qualified Example.PgQuery.Internal.DB as DB

new :: IO DB.Handle
new =
  pure DB.Handle
    { DB.insertTwoMessages
    , DB.fetchTwoMessages

    , DB.close = mempty
    }

withHandle :: (DB.Handle -> IO a) -> IO a
withHandle = Exception.bracket new DB.close

insertTwoMessages
  :: String -> String -> Tx.PgQueryM (Int, Int)
insertTwoMessages s1 s2 = do
  Tx.pgQuery [Tx.sqlExp|
    insert into foo(message) values (#{s1}), (#{s2}) returning id
  |] >>= \case
    [Tx.Only k1, Tx.Only k2] -> pure (k1, k2)
    rows -> error $ "Expected exactly 2 rows, got " <> show (length rows)

fetchTwoMessages
  :: Int -> Int -> Tx.PgQueryM (Maybe String, Maybe String)
fetchTwoMessages k1 k2 = do
  rows <- Tx.pgQuery [Tx.sqlExp|
    select id, message
    from foo
    where id in #{Tx.In [k1, k2]}
  |]
  pure (lookup k1 rows, lookup k2 rows)
