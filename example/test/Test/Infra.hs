{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Infra where

import Control.Exception (try)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT(LoggingT), runStderrLoggingT)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Proxy (Proxy(Proxy))
import Database.PostgreSQL.Tx (TxException(TxException), TxM, throwExceptionTx)
import Database.PostgreSQL.Tx.HEnv (HEnv)
import Database.PostgreSQL.Tx.Query (Logger, LibPQConnection, PgSimpleConnection)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Test.Hspec (HasCallStack, Spec, describe, expectationFailure, it, pendingWith, shouldBe, shouldReturn)
import qualified Database.PostgreSQL.Simple as PG.Simple
import qualified Database.PostgreSQL.Tx as Tx
import qualified Database.PostgreSQL.Tx.HEnv as HEnv
import qualified Database.PostgreSQL.Tx.Query as Tx.Query
import qualified Database.PostgreSQL.Tx.Unsafe as Tx.Unsafe
import qualified Example.PgQuery
import qualified Example.PgSimple
import qualified Example.Squeal

testBackend :: (KnownSymbol backend) => Backend backend AppEnv tm -> Spec
testBackend backend = testBackend' backend (pure ())

testBackend'
  :: forall backend tm. (KnownSymbol backend)
  => Backend backend AppEnv tm -> Spec -> Spec
testBackend' Backend {..} extraTests = do
  describe (symbolVal (Proxy @backend)) do
    generatedTests
    extraTests
  where
  raiseErrCode errCode = do
    raiseException
      ("Raising exception via " <> symbolVal (Proxy @backend)
        <> " with error code '" <> errCode <> "'")
      (Just errCode)

  generatedTests = do
    describe "transaction runners" do
      it "withTransaction" do
        demoTest (withTransaction)
      it "withTransactionMode RepeatableRead" do
        demoTest (withTransactionMode transactionMode'RepeatableRead)
      describe "withTransactionSerializable" do
        it "retries when appropriate" $ testCase \appEnv -> do
          counter <- newIORef (0 :: Int)
          withTransactionSerializable appEnv do
            -- Increment our counter and get its new value.
            n <- Tx.Unsafe.unsafeRunIOInTxM do
              atomicModifyIORef' counter \i -> let j = i + 1 in (j, j)
            case n of
              0 -> raiseErrCode "serialization_failure"
              1 -> raiseErrCode "deadlock_detected"
              _ -> pure ()
          readIORef counter `shouldReturn` 2
        it "does not retry when not appropriate" $ testCase \appEnv -> do
          expectTxErrcode "23503" do
            withTransactionSerializable appEnv do
              raiseErrCode "foreign_key_violation"
    describe "TxException" do
      it "wraps serialization_failure" $ testCase \appEnv -> do
        expectTxErrcode "40001" do
          withTransaction appEnv do
            raiseErrCode "serialization_failure"
      it "wraps deadlock_detected" $ testCase \appEnv -> do
        expectTxErrcode "40P01" do
          withTransaction appEnv do
            raiseErrCode "deadlock_detected"
      it "wraps other applicable errors" $ testCase \appEnv -> do
        expectTxErrcode "23514" do
          withTransaction appEnv do
            raiseErrCode "check_violation"
      it "wraps applicable errors with no specified error code" $ testCase \appEnv -> do
        expectTxErrcode "P0001" do
          withTransaction appEnv do
            raiseException "oh noes" Nothing
      it "doesn't wrap inapplicable exceptions" $ testCase \appEnv -> do
        let e = userError "boom"
        let go = void $ withTransaction appEnv $ throwExceptionTx e
        try go `shouldReturn` Left e

demoTest :: (forall a. AppEnv -> AppM a -> IO a) -> IO ()
demoTest runTransaction = testCase \appEnv -> do
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

type AppM = TxM AppEnv

type AppEnv =
  HEnv
    '[ PgSimpleConnection
     , LibPQConnection
     , Logger
     ]

withAppEnv :: PG.Simple.Connection -> Logger -> (AppEnv -> IO a) -> IO a
withAppEnv simpleConn logger = do
  Tx.Query.usingPgSimpleConnection
    simpleConn
    (HEnv.singleton logger)

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

testCase :: (AppEnv -> IO a) -> IO a
testCase f = do
  conn <- PG.Simple.connectPostgreSQL "dbname=postgresql-tx-example"
  let logger = toLogger runStderrLoggingT
  withAppEnv conn logger \env -> do
    Tx.Unsafe.unsafeRunTxM env do
      void $ Tx.Query.pgExecute [Tx.Query.sqlExp|
        drop table if exists foo
      |]
      void $ Tx.Query.pgExecute [Tx.Query.sqlExp|
        create table foo
          ( id serial primary key
          , message text not null unique
          )
      |]
      void $ Tx.Query.pgExecute [Tx.Query.sqlExp|
        drop function if exists raise_exception(text, text);
      |]
      void $ Tx.Query.pgExecute [Tx.Query.sqlExp|
        create function raise_exception(message text, error_code text) returns void as $$
          begin
            if error_code is null then
              raise exception '%', message;
            else
              raise exception '%', message using errcode = error_code;
            end if;
          end;
        $$ language plpgsql;
      |]
    f env

expectTxErrcode :: (HasCallStack) => String -> IO () -> IO ()
expectTxErrcode e io = do
  try io >>= \case
    Right _ -> expectationFailure "No exception was thrown"
    Left TxException { errcode } -> errcode `shouldBe` Just e

toLogger :: (LoggingT IO () -> IO ()) -> Logger
toLogger f loc src lvl msg =
  f $ LoggingT \logger -> liftIO $ logger loc src lvl msg

-- | Can be used in a field definition for 'Backend' in the event
-- that the backend does not yet support some feature without
-- breaking the test suite.
throwPendingIO :: String -> IO a
throwPendingIO msg = pendingWith msg >> error "can't get here"

-- | Same as 'throwPendingIO' except works for 'TxM'.
throwPendingTx :: String -> TxM r a
throwPendingTx = Tx.Unsafe.unsafeRunIOInTxM . throwPendingIO

-- | Generalized interface for testing a @postgresql-tx@ backend.
data Backend (backend :: Symbol) r tm = Backend
  { -- | Raises an exception from postgresql with the given @message@ and
    -- optional @errcode@.
    raiseException :: String -> Maybe String -> TxM r ()

    -- | Runs a transaction at the default mode.
  , withTransaction :: forall a. r -> TxM r a -> IO a
    -- | Runs a transaction at the specified mode @tm@.
  , withTransactionMode :: forall a. tm -> r -> TxM r a -> IO a
    -- | Runs a transaction at @serializable@ with retry.
  , withTransactionSerializable :: forall a. r -> TxM r a -> IO a

    -- | A transaction mode of @repeatable read@.
  , transactionMode'RepeatableRead :: tm
  }
