{-# LANGUAGE BlockArguments #-}
module Database.PostgreSQL.Tx
  ( -- * Introduction
    -- $intro

    -- ** Transaction monad
    TxM

    -- ** Transaction environment
  , TxEnv(withTxEnv)
  , TxEnvs
  , withTxEnv'Selecting
  , withTxEnv'Resource
  , withTxEnv'Singleton

    -- ** Exceptions
  , throwExceptionTx
  , mapExceptionTx
  ) where

import Control.Exception (Exception, catch, throwIO)
import Database.PostgreSQL.Tx.Internal

-- | Throw an exception.
--
-- This function may be used within 'TxM' or a specific database library
-- implementation monad from the various @postgresql-tx-*@ packages.
--
-- @since 0.2.0.0
throwExceptionTx :: (Exception e) => e -> TxM r a
throwExceptionTx = unsafeRunIOInTxM . throwIO

-- | Catch an exception and map it to another exception type before rethrowing.
--
-- This function may be used within 'TxM' or a specific database library
-- implementation monad from the various @postgresql-tx-*@ packages.
--
-- @since 0.2.0.0
mapExceptionTx
  :: (Exception e, Exception e')
  => (e -> Maybe e')
  -> TxM r a
  -> TxM r a
mapExceptionTx mapper action = do
  unsafeWithRunInIOTxM \run -> do
    catch (run action) \ex -> do
      case mapper ex of
        Nothing -> throwIO ex
        Just ex' -> throwIO ex'

-- $intro
--
-- @postgresql-tx@ provides a transaction monad - 'TxM'. When arbitrary 'IO' is
-- attempted to be performed within 'TxM', a type error is generated. Arbitrary
-- 'IO' can still be performed when neccessary, but users must explicitly
-- opt-in to this via the "Database.PostgreSQL.Tx.Unsafe" module's @unsafe*@
-- functions.
--
-- Note that @posgresql-tx@ has no dependencies on any specific
-- @postgres@-related database libraries. This library defines the 'TxM' monad
-- and the infrastructure necessary to adapt specific database libraries for
-- 'TxM' compatiblity. The idea is that an adaptor library provides a means to
-- convert a specific database library's implementation monad into 'TxM'.
-- Application authors can then freely mix 'TxM' database functions together
-- even if the implementations of these database functions are using different
-- underlying database libraries, e.g. @postgresql-query@ , @squeal-postgresql@,
-- @postgresql-simple@, etc.
