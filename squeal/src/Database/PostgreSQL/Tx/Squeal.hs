{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Database.PostgreSQL.Tx.Squeal
  ( SquealEnv
  , SquealM
  , SquealTxM'(SquealTxM, fromSquealTxM)
  , SquealTxM
  , SquealConnection
  , mkSquealConnection
  , module Database.PostgreSQL.Tx.Squeal
  , module Database.PostgreSQL.Tx.Squeal.Internal.Reexport
  ) where

import Data.ByteString (ByteString)
import Database.PostgreSQL.Tx (TxM)
import Database.PostgreSQL.Tx.Squeal.Internal
import Database.PostgreSQL.Tx.Squeal.Internal.Reexport
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Generics.SOP as SOP
import qualified Generics.SOP.Record as SOP
import qualified Squeal.PostgreSQL as Squeal

-- | Analogue of 'Squeal.getRow'.
--
-- @since 0.1.0.0
getRow :: LibPQ.Row -> Result y -> SquealTxM db y
getRow = unsafeSquealIOTxM2 Squeal.getRow

-- | Analogue of 'Squeal.firstRow'.
--
-- @since 0.1.0.0
firstRow :: Result y -> SquealTxM db (Maybe y)
firstRow = unsafeSquealIOTxM1 Squeal.firstRow

-- | Analogue of 'Squeal.getRows'.
--
-- @since 0.1.0.0
getRows :: Result y -> SquealTxM db [y]
getRows = unsafeSquealIOTxM1 Squeal.getRows

-- | Analogue of 'Squeal.nextRow'.
--
-- @since 0.1.0.0
nextRow :: LibPQ.Row -> Result y -> LibPQ.Row -> SquealTxM db (Maybe (LibPQ.Row, y))
nextRow = unsafeSquealIOTxM3 Squeal.nextRow

-- | Analogue of 'Squeal.ntuples'.
--
-- @since 0.1.0.0
ntuples :: Result y -> SquealTxM db LibPQ.Row
ntuples = unsafeSquealIOTxM1 Squeal.ntuples

-- | Analogue of 'Squeal.nfields'.
--
-- @since 0.1.0.0
nfields :: Result y -> SquealTxM db LibPQ.Column
nfields = unsafeSquealIOTxM1 Squeal.nfields

-- | Analogue of 'Squeal.resultStatus'.
--
-- @since 0.1.0.0
resultStatus :: Result y -> SquealTxM db ExecStatus
resultStatus = unsafeSquealIOTxM1 Squeal.resultStatus

-- | Analogue of 'Squeal.okResult'.
--
-- @since 0.1.0.0
okResult :: K LibPQ.Result row -> SquealTxM db ()
okResult = unsafeSquealIOTxM1 Squeal.okResult

-- | Analogue of 'Squeal.resultErrorMessage'.
--
-- @since 0.1.0.0
resultErrorMessage :: Result y -> SquealTxM db (Maybe ByteString)
resultErrorMessage = unsafeSquealIOTxM1 Squeal.resultErrorMessage

-- | Analogue of 'Squeal.resultErrorCode'.
--
-- @since 0.1.0.0
resultErrorCode :: Result y -> SquealTxM db (Maybe ByteString)
resultErrorCode = unsafeSquealIOTxM1 Squeal.resultErrorCode

-- | Analogue of 'Squeal.executeParams'.
--
-- @since 0.1.0.0
executeParams :: Statement db x y -> x -> SquealTxM db (Result y)
executeParams = unsafeSquealIOTxM2 Squeal.executeParams

-- | Analogue of 'Squeal.executeParams_'.
--
-- @since 0.1.0.0
executeParams_ :: Statement db x () -> x -> SquealTxM db ()
executeParams_ = unsafeSquealIOTxM2 Squeal.executeParams_

-- | Analogue of 'Squeal.execute'.
--
-- @since 0.1.0.0
execute :: Statement db () y -> SquealTxM db (Result y)
execute = unsafeSquealIOTxM1 Squeal.execute

-- | Analogue of 'Squeal.execute_'.
--
-- @since 0.1.0.0
execute_ :: Statement db () () -> SquealTxM db ()
execute_ = unsafeSquealIOTxM1 Squeal.execute_

-- | Analogue of 'Squeal.executePrepared'.
--
-- @since 0.1.0.0
executePrepared
  :: (Traversable list)
  => Statement db x y -> list x -> SquealTxM db (list (Result y))
executePrepared = unsafeSquealIOTxM2 Squeal.executePrepared

-- | Analogue of 'Squeal.executePrepared_'.
--
-- @since 0.1.0.0
executePrepared_
  :: (Foldable list)
  => Statement db x () -> list x -> SquealTxM db ()
executePrepared_ = unsafeSquealIOTxM2 Squeal.executePrepared_

-- | Analogue of 'Squeal.manipulateParams'.
--
-- @since 0.1.0.0
manipulateParams
  :: ( GenericParams db params x xs
     , Squeal.GenericRow row y ys
     )
  => Manipulation '[] db params row
  -> x
  -> SquealTxM db (Result y)
manipulateParams = unsafeSquealIOTxM2 Squeal.manipulateParams

-- | Analogue of 'Squeal.manipulateParams_'.
--
-- @since 0.1.0.0
manipulateParams_
  :: (GenericParams db params x xs)
  => Manipulation '[] db params '[] -> x -> SquealTxM db ()
manipulateParams_ = unsafeSquealIOTxM2 Squeal.manipulateParams_

-- | Analogue of 'Squeal.manipulate'.
--
-- @since 0.1.0.0
manipulate
  :: (Squeal.GenericRow row y ys)
  => Manipulation '[] db '[] row -> SquealTxM db (Result y)
manipulate = unsafeSquealIOTxM1 Squeal.manipulate

-- | Analogue of 'Squeal.manipulate_'.
--
-- @since 0.1.0.0
manipulate_
  :: Manipulation '[] db '[] '[] -> SquealTxM db ()
manipulate_ = unsafeSquealIOTxM1 Squeal.manipulate_

-- | Analogue of 'Squeal.runQueryParams'.
--
-- @since 0.1.0.0
runQueryParams
  :: ( GenericParams db params x xs
     , SOP.IsRecord y ys
     , SOP.AllZip Squeal.FromField row ys
     )
  => Squeal.Query '[] '[] db params row -> x -> SquealTxM db (Result y)
runQueryParams = unsafeSquealIOTxM2 Squeal.runQueryParams

-- | Analogue of 'Squeal.runQuery'.
--
-- @since 0.1.0.0
runQuery
  :: ( SOP.IsRecord y ys
     , SOP.AllZip Squeal.FromField row ys
     )
  => Squeal.Query '[] '[] db '[] row -> SquealTxM db (Result y)
runQuery = unsafeSquealIOTxM1 Squeal.runQuery

-- | Analogue of 'Squeal.traversePrepared'.
--
-- @since 0.1.0.0
traversePrepared
  :: ( GenericParams db params x xs
     , Traversable list
     , SOP.IsRecord y ys
     , SOP.AllZip Squeal.FromField row ys
     )
  => Manipulation '[] db params row
  -> list x
  -> SquealTxM db (list (Result y))
traversePrepared = unsafeSquealIOTxM2 Squeal.traversePrepared

-- | Analogue of 'Squeal.forPrepared'.
--
-- @since 0.1.0.0
forPrepared
  :: ( GenericParams db params x xs
     , Traversable list
     , SOP.IsRecord y ys
     , SOP.AllZip Squeal.FromField row ys
     )
  => list x
  -> Manipulation '[] db params row
  -> SquealTxM db (list (Result y))
forPrepared = unsafeSquealIOTxM2 Squeal.forPrepared

-- | Analogue of 'Squeal.traversePrepared_'.
--
-- @since 0.1.0.0
traversePrepared_
  :: ( GenericParams db params x xs
     , Foldable list
     )
  => Manipulation '[] db params '[]
  -> list x
  -> SquealTxM db ()
traversePrepared_ = unsafeSquealIOTxM2 Squeal.traversePrepared_

-- | Analogue of 'Squeal.forPrepared_'.
--
-- @since 0.1.0.0
forPrepared_
  :: ( GenericParams db params x xs
     , Foldable list
     )
  => list x
  -> Manipulation '[] db params '[]
  -> SquealTxM db ()
forPrepared_ = unsafeSquealIOTxM2 Squeal.forPrepared_

-- | Analogue of 'Squeal.transactionally'.
--
-- @since 0.1.0.0
transactionally :: (SquealEnv r) => TransactionMode -> r -> TxM r a -> IO a
transactionally m = unsafeRunSquealTransaction (Squeal.transactionally m)

-- | Analogue of 'Squeal.transactionally_'.
--
-- @since 0.1.0.0
transactionally_ :: (SquealEnv r) => r -> TxM r a -> IO a
transactionally_ = unsafeRunSquealTransaction Squeal.transactionally_

-- TODO: Removed for now until we provide a retry mechanism.
-- -- | Analogue of 'Squeal.transactionallyRetry'.
-- --
-- -- @since 0.1.0.0
-- transactionallyRetry :: (SquealEnv r) => TransactionMode -> r -> TxM r a -> IO a
-- transactionallyRetry m = unsafeRunSquealTransaction (Squeal.transactionallyRetry m)

-- | Analogue of 'Squeal.ephemerally'.
--
-- @since 0.1.0.0
ephemerally :: (SquealEnv r) => TransactionMode -> r -> TxM r a -> IO a
ephemerally m = unsafeRunSquealTransaction (Squeal.ephemerally m)

-- | Analogue of 'Squeal.ephemerally_'.
--
-- @since 0.1.0.0
ephemerally_ :: (SquealEnv r) => r -> TxM r a -> IO a
ephemerally_ = unsafeRunSquealTransaction Squeal.ephemerally_
