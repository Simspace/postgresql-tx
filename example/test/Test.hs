{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import Control.Concurrent (withMVar)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT(LoggingT), runStderrLoggingT)
import Database.PostgreSQL.LibPQ as LibPQ
import Database.PostgreSQL.Tx (TxEnv(withTxEnv), TxM, withTxEnv'Resource, withTxEnv'Selecting, withTxEnv'Singleton)
import Database.PostgreSQL.Tx.Query (Logger)
import Database.PostgreSQL.Tx.Squeal (SquealSchemas(SquealSchemas))
import GHC.Stack (HasCallStack)
import qualified Database.PostgreSQL.Simple as PG.Simple
import qualified Database.PostgreSQL.Simple.Internal as PG.Simple.Internal
import qualified Database.PostgreSQL.Tx.Query as Tx.Query
import qualified Example.PgQuery
import qualified Example.PgSimple
import qualified Example.Squeal

main :: IO ()
main = do
  dbEnv <- initDBEnv

      -- For this demo we are using postgresql-query for handling transactions;
      -- however, we could easily swap this out with any other postgresql-tx
      -- supported library.
  let runTransaction :: AppM a -> IO a
      runTransaction =
        Tx.Query.pgWithTransactionMode
          Tx.Query.defaultTransactionMode
          dbEnv

  (ms1, ms2, ms3, ms4, ms5, ms6) <- do
    Example.PgSimple.withHandle \pgSimpleDB -> do
      Example.PgQuery.withHandle \pgQueryDB -> do
        Example.Squeal.withHandle \squealDB -> do
          runTransaction do
            demo pgSimpleDB pgQueryDB squealDB

  ms1 `shouldBe` Just "pg-simple: hi"
  ms2 `shouldBe` Just "pg-query: sup"
  ms3 `shouldBe` Just "pg-query: wut"
  ms4 `shouldBe` Just "squeal: nuthin"
  ms5 `shouldBe` Just "squeal: ye"
  ms6 `shouldBe` Just "squeal: k bye"
  putStrLn "Success!"
  where
  shouldBe :: (HasCallStack, Eq a, Show a) => a -> a -> IO ()
  x `shouldBe` y =
    when (x /= y) do
      error $ show x <> " /= " <> show y

type AppM = TxM DBEnv

data DBEnv = DBEnv
  { dbConnection :: PG.Simple.Connection
  , dbLogger :: Logger
  }

instance TxEnv DBEnv PG.Simple.Connection where
  withTxEnv = withTxEnv'Selecting dbConnection

instance TxEnv DBEnv LibPQ.Connection where
  withTxEnv = withTxEnv'Resource withLibPQConnection

instance TxEnv DBEnv Logger where
  withTxEnv = withTxEnv'Selecting dbLogger

instance (s ~ Example.Squeal.Schemas) => TxEnv DBEnv (SquealSchemas s s) where
  withTxEnv = withTxEnv'Singleton (SquealSchemas @s @s)

demo
  :: Example.PgSimple.Handle
  -> Example.PgQuery.Handle
  -> Example.Squeal.Handle
  -> AppM
      ( Maybe String
      , Maybe String
      , Maybe String
      , Maybe String
      , Maybe String
      , Maybe String
      )
demo pgSimpleDB pgQueryDB squealDB = do
  k1 <- Example.PgSimple.insertMessage pgSimpleDB "pg-simple: hi"
  (k2, k3) <- Example.PgQuery.insertTwoMessages pgQueryDB "pg-query: sup" "pg-query: wut"
  ms2 <- Example.PgSimple.fetchMessage pgSimpleDB k2
  (ms1, ms3) <- Example.PgQuery.fetchTwoMessages pgQueryDB k1 k3
  (k4, k5, k6) <- Example.Squeal.insertThreeMessages squealDB "squeal: nuthin" "squeal: ye" "squeal: k bye"
  (ms4, ms5, ms6) <- Example.Squeal.fetchThreeMessages squealDB k4 k5 k6
  pure (ms1, ms2, ms3, ms4, ms5, ms6)

initDBEnv :: IO DBEnv
initDBEnv = do
  conn <- PG.Simple.connectPostgreSQL "dbname=postgresql-tx-example"
  _ <- PG.Simple.execute_ conn "drop table if exists foo"
  _ <- PG.Simple.execute_ conn $
        "create table if not exists foo"
          <> "( id serial primary key"
          <> ", message text not null unique"
          <> ")"
  pure DBEnv
    { dbConnection = conn
    , dbLogger = toLogger runStderrLoggingT
    }

withLibPQConnection :: DBEnv -> (LibPQ.Connection -> IO x) -> IO x
withLibPQConnection DBEnv { dbConnection } f = do
  withMVar (PG.Simple.Internal.connectionHandle dbConnection) f

toLogger :: (LoggingT IO () -> IO ()) -> Logger
toLogger f loc src lvl msg =
  f $ LoggingT \logger -> liftIO $ logger loc src lvl msg
