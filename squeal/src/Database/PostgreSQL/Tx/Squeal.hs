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
  , SquealSchemas(SquealSchemas)
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
getRow :: forall db r y. (SquealEnv db db r) => LibPQ.Row -> Result y -> TxM r y
getRow = unsafeSquealIOTx2 @db Squeal.getRow

-- | Analogue of 'Squeal.firstRow'.
--
-- @since 0.1.0.0
firstRow :: forall db r y. (SquealEnv db db r) => Result y -> TxM r (Maybe y)
firstRow = unsafeSquealIOTx1 @db Squeal.firstRow

-- | Analogue of 'Squeal.getRows'.
--
-- @since 0.1.0.0
getRows :: forall db r y. (SquealEnv db db r) => Result y -> TxM r [y]
getRows = unsafeSquealIOTx1 @db Squeal.getRows

-- | Analogue of 'Squeal.nextRow'.
--
-- @since 0.1.0.0
nextRow :: forall db r y. (SquealEnv db db r) => LibPQ.Row -> Result y -> LibPQ.Row -> TxM r (Maybe (LibPQ.Row, y))
nextRow = unsafeSquealIOTx3 @db Squeal.nextRow

-- | Analogue of 'Squeal.ntuples'.
--
-- @since 0.1.0.0
ntuples :: forall db r y. (SquealEnv db db r) => Result y -> TxM r LibPQ.Row
ntuples = unsafeSquealIOTx1 @db Squeal.ntuples

-- | Analogue of 'Squeal.nfields'.
--
-- @since 0.1.0.0
nfields :: forall db r y. (SquealEnv db db r) => Result y -> TxM r LibPQ.Column
nfields = unsafeSquealIOTx1 @db Squeal.nfields

-- | Analogue of 'Squeal.resultStatus'.
--
-- @since 0.1.0.0
resultStatus :: forall db r y. (SquealEnv db db r) => Result y -> TxM r ExecStatus
resultStatus = unsafeSquealIOTx1 @db Squeal.resultStatus

-- | Analogue of 'Squeal.okResult'.
--
-- @since 0.1.0.0
okResult :: forall db r row. (SquealEnv db db r) => K LibPQ.Result row -> TxM r ()
okResult = unsafeSquealIOTx1 @db Squeal.okResult

-- | Analogue of 'Squeal.resultErrorMessage'.
--
-- @since 0.1.0.0
resultErrorMessage :: forall db r y. (SquealEnv db db r) => Result y -> TxM r (Maybe ByteString)
resultErrorMessage = unsafeSquealIOTx1 @db Squeal.resultErrorMessage

-- | Analogue of 'Squeal.resultErrorCode'.
--
-- @since 0.1.0.0
resultErrorCode :: forall db r y. (SquealEnv db db r) => Result y -> TxM r (Maybe ByteString)
resultErrorCode = unsafeSquealIOTx1 @db Squeal.resultErrorCode

-- | Analogue of 'Squeal.executeParams'.
--
-- @since 0.1.0.0
executeParams :: forall db r x y. (SquealEnv db db r) => Statement db x y -> x -> TxM r (Result y)
executeParams = unsafeSquealIOTx2 @db Squeal.executeParams

-- | Analogue of 'Squeal.executeParams_'.
--
-- @since 0.1.0.0
executeParams_ :: forall db r x. (SquealEnv db db r) => Statement db x () -> x -> TxM r ()
executeParams_ = unsafeSquealIOTx2 @db Squeal.executeParams_

-- | Analogue of 'Squeal.execute'.
--
-- @since 0.1.0.0
execute :: forall db r y. (SquealEnv db db r) => Statement db () y -> TxM r (Result y)
execute = unsafeSquealIOTx1 @db Squeal.execute

-- | Analogue of 'Squeal.execute_'.
--
-- @since 0.1.0.0
execute_ :: forall db r. (SquealEnv db db r) => Statement db () () -> TxM r ()
execute_ = unsafeSquealIOTx1 @db Squeal.execute_

-- | Analogue of 'Squeal.executePrepared'.
--
-- @since 0.1.0.0
executePrepared
  :: (Traversable list)
  => Statement db x y -> list x -> SquealM db (list (Result y))
executePrepared = unsafeSquealIOTx2 Squeal.executePrepared

-- | Analogue of 'Squeal.executePrepared_'.
--
-- @since 0.1.0.0
executePrepared_
  :: (Foldable list)
  => Statement db x () -> list x -> SquealM db ()
executePrepared_ = unsafeSquealIOTx2 Squeal.executePrepared_

-- | Analogue of 'Squeal.manipulateParams'.
--
-- @since 0.1.0.0
manipulateParams
  :: ( GenericParams db params x xs
     , Squeal.GenericRow row y ys
     )
  => Manipulation '[] db params row
  -> x
  -> SquealM db (Result y)
manipulateParams = unsafeSquealIOTx2 Squeal.manipulateParams

-- | Analogue of 'Squeal.manipulateParams_'.
--
-- @since 0.1.0.0
manipulateParams_
  :: (GenericParams db params x xs)
  => Manipulation '[] db params '[] -> x -> SquealM db ()
manipulateParams_ = unsafeSquealIOTx2 Squeal.manipulateParams_

-- | Analogue of 'Squeal.manipulate'.
--
-- @since 0.1.0.0
manipulate
  :: (Squeal.GenericRow row y ys)
  => Manipulation '[] db '[] row -> SquealM db (Result y)
manipulate = unsafeSquealIOTx1 Squeal.manipulate

-- | Analogue of 'Squeal.manipulate_'.
--
-- @since 0.1.0.0
manipulate_
  :: Manipulation '[] db '[] '[] -> SquealM db ()
manipulate_ = unsafeSquealIOTx1 Squeal.manipulate_

-- | Analogue of 'Squeal.runQueryParams'.
--
-- @since 0.1.0.0
runQueryParams
  :: ( GenericParams db params x xs
     , SOP.IsRecord y ys
     , SOP.AllZip Squeal.FromField row ys
     )
  => Squeal.Query '[] '[] db params row -> x -> SquealM db (Result y)
runQueryParams = unsafeSquealIOTx2 Squeal.runQueryParams

-- | Analogue of 'Squeal.runQuery'.
--
-- @since 0.1.0.0
runQuery
  :: ( SOP.IsRecord y ys
     , SOP.AllZip Squeal.FromField row ys
     )
  => Squeal.Query '[] '[] db '[] row -> SquealM db (Result y)
runQuery = unsafeSquealIOTx1 Squeal.runQuery

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
  -> SquealM db (list (Result y))
traversePrepared = unsafeSquealIOTx2 Squeal.traversePrepared

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
  -> SquealM db (list (Result y))
forPrepared = unsafeSquealIOTx2 Squeal.forPrepared

-- | Analogue of 'Squeal.traversePrepared_'.
--
-- @since 0.1.0.0
traversePrepared_
  :: ( GenericParams db params x xs
     , Foldable list
     )
  => Manipulation '[] db params '[]
  -> list x
  -> SquealM db ()
traversePrepared_ = unsafeSquealIOTx2 Squeal.traversePrepared_

-- | Analogue of 'Squeal.forPrepared_'.
--
-- @since 0.1.0.0
forPrepared_
  :: ( GenericParams db params x xs
     , Foldable list
     )
  => list x
  -> Manipulation '[] db params '[]
  -> SquealM db ()
forPrepared_ = unsafeSquealIOTx2 Squeal.forPrepared_

-- | Analogue of 'Squeal.transactionally'.
--
-- @since 0.1.0.0
transactionally :: forall db r a. (SquealEnv db db r) => TransactionMode -> r -> TxM r a -> IO a
transactionally m = unsafeRunSquealTransaction @db (Squeal.transactionally @_ @db m)

-- | Analogue of 'Squeal.transactionally_'.
--
-- @since 0.1.0.0
transactionally_ :: forall db r a. (SquealEnv db db r) => r -> TxM r a -> IO a
transactionally_ = unsafeRunSquealTransaction @db Squeal.transactionally_

-- | Analogue of 'Squeal.transactionallyRetry'.
--
-- @since 0.1.0.0
transactionallyRetry :: forall db r a. (SquealEnv db db r) => TransactionMode -> r -> TxM r a -> IO a
transactionallyRetry m = unsafeRunSquealTransaction @db (Squeal.transactionallyRetry m)

-- | Analogue of 'Squeal.ephemerally'.
--
-- @since 0.1.0.0
ephemerally :: forall db r a. (SquealEnv db db r) => TransactionMode -> r -> TxM r a -> IO a
ephemerally m = unsafeRunSquealTransaction @db (Squeal.ephemerally m)

-- | Analogue of 'Squeal.ephemerally_'.
--
-- @since 0.1.0.0
ephemerally_ :: forall db r a. (SquealEnv db db r) => r -> TxM r a -> IO a
ephemerally_ = unsafeRunSquealTransaction @db Squeal.ephemerally_
