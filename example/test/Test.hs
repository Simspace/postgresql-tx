{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT(LoggingT), runStderrLoggingT)
import Database.PostgreSQL.Tx (TxM)
import Database.PostgreSQL.Tx.HEnv (HEnv)
import Database.PostgreSQL.Tx.Query (Logger)
import Database.PostgreSQL.Tx.Squeal (SquealConnection)
import Test.Hspec
import qualified Database.PostgreSQL.Simple as PG.Simple
import qualified Database.PostgreSQL.Tx.HEnv as HEnv
import qualified Database.PostgreSQL.Tx.Query as Tx.Query
import qualified Database.PostgreSQL.Tx.Squeal as Tx.Squeal
import qualified Database.PostgreSQL.Tx.Squeal.Compat.Simple as Tx.Squeal.Compat.Simple
import qualified Database.PostgreSQL.Tx.Unsafe as Tx.Unsafe
import qualified Example.PgQuery
import qualified Example.PgSimple
import qualified Example.Squeal

main :: IO ()
main = hspec do
  describe "postgresql-tx transaction runners" do

    describe "postgresql-tx-query" do
      it "supports pgWithTransaction" do
        theTest Tx.Query.pgWithTransaction
      it "supports pgWithTransactionMode" do
        theTest $ Tx.Query.pgWithTransactionMode Tx.Query.defaultTransactionMode

    describe "postgresql-tx-squeal" do
      it "supports transactionally_" do
        theTest Tx.Squeal.transactionally_
      it "supports transactionally" do
        theTest $ Tx.Squeal.transactionally Tx.Squeal.defaultMode
  where
  theTest :: (forall a. AppEnv -> AppM a -> IO a) -> IO ()
  theTest runTransaction = withAppEnv \appEnv -> do
    (ms1, ms2, ms3, ms4, ms5, ms6) <- do
      Example.PgSimple.withHandle \pgSimpleDB -> do
        Example.PgQuery.withHandle \pgQueryDB -> do
          Example.Squeal.withHandle \squealDB -> do
            runTransaction appEnv do
              demo pgSimpleDB pgQueryDB squealDB
    ms1 `shouldBe` Just "pg-simple: hi"
    ms2 `shouldBe` Just "pg-query: sup"
    ms3 `shouldBe` Just "pg-query: wut"
    ms4 `shouldBe` Just "squeal: nuthin"
    ms5 `shouldBe` Just "squeal: ye"
    ms6 `shouldBe` Just "squeal: k bye"
    putStrLn "Success!"

type AppM = TxM AppEnv

type AppEnv =
  HEnv
    '[ PG.Simple.Connection
     , Logger
     , SquealConnection
     ]

demo
  :: Example.PgSimple.Handle AppM
  -> Example.PgQuery.Handle AppM
  -> Example.Squeal.Handle AppM
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

withAppEnv :: (AppEnv -> IO a) -> IO a
withAppEnv f = do
  conn <- PG.Simple.connectPostgreSQL "dbname=postgresql-tx-example"
  withEnv conn \env -> do
    Tx.Unsafe.unsafeRunTxM env do
      void $ Tx.Query.pgExecute [Tx.Query.sqlExp|
        drop table if exists foo
      |]
      void $ Tx.Query.pgExecute [Tx.Query.sqlExp|
        create table if not exists foo
          ( id serial primary key
          , message text not null unique
          )
      |]
    f env
  where
  logger = toLogger runStderrLoggingT

  withEnv simpleConn g = do
    Tx.Squeal.Compat.Simple.withSquealConnection simpleConn \squealConn -> do
      g $ HEnv.fromTuple
        ( simpleConn
        , logger
        , squealConn
        )

toLogger :: (LoggingT IO () -> IO ()) -> Logger
toLogger f loc src lvl msg =
  f $ LoggingT \logger -> liftIO $ logger loc src lvl msg
