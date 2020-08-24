module Database.PostgreSQL.Tx.Unsafe
  ( -- * Introduction
    -- $intro

    -- ** Operations
    unsafeRunIOInTxM
  , unsafeWithRunInIOTxM

    -- ** For adaptor libraries
  , unsafeUnTxM
  , unsafeRunTxM

  , unsafeMkTxM
  , unsafeMksTxM

  , unsafeWithTxEnvIO
  ) where

import Database.PostgreSQL.Tx.Internal

-- $intro
--
-- @postgresql-tx@ discourages performing arbitrary 'IO' within a database
-- transaction, but sometimes performing this 'IO' is necessary. This module
-- provides operations and infrastructure for performing "unsafe" 'IO' actions
-- within 'TxM' or a specific database library implementation monad. It also
-- provides utilities for use in adaptor libraries.
--
-- Clients must explicitly import this module to perform 'IO' in 'TxM' or a
-- specific database library implementation monad. All functions this module
-- provides are prefixed with @unsafe*@. These two factors serve as annotation
-- to simplify understanding exactly which parts of transactional database code
-- are performing arbitary 'IO'.
