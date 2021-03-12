{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.PostgreSQL.Tx.Random.Internal where

import Control.Arrow (first)
import Control.Monad.Random.Class (MonadRandom(getRandom, getRandomR, getRandomRs, getRandoms))
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Kind (Constraint)
import Data.Tuple (swap)
import Database.PostgreSQL.Tx (TxEnv, TxM, askTxEnv)
import Database.PostgreSQL.Tx.HEnv (HEnv)
import Database.PostgreSQL.Tx.Unsafe (unsafeRunIOInTxM)
import System.Random (Random(random, randomR, randomRs, randoms), RandomGen(split), StdGen, newStdGen)
import qualified Database.PostgreSQL.Tx.HEnv as HEnv

-- | Runtime environment needed to use @random@ functions via @postgresql-tx@.
--
-- @since 0.2.0.0
type RandomEnv r = (TxEnv TxGen r) :: Constraint

-- | Same as 'getRandom' but specialized for 'TxM'.
--
-- @since 0.2.0.0
randomTxM :: (Random a, RandomEnv r) => TxM r a
randomTxM = getRandom

-- | Same as 'getRandomR' but specialized for 'TxM'.
--
-- @since 0.2.0.0
randomRTxM :: (Random a, RandomEnv r) => (a, a) -> TxM r a
randomRTxM = getRandomR

-- | Runner required for using 'MonadRandom' functions in 'TxM'.
--
-- @since 0.2.0.0
newtype TxGen = TxGen { getTxRandom' :: forall a r. (StdGen -> (a, StdGen)) -> TxM r a }

-- | Manages the 'StdGen' state for a 'TxM'.
-- Analogous to 'System.Random.getStdRandom'.
--
-- @since 0.2.0.0
getTxRandom :: (RandomEnv r) => (StdGen -> (a, StdGen)) -> TxM r a
getTxRandom f = do
  TxGen g <- askTxEnv
  g f

-- | Adds the given 'StdGen' as a 'TxGen' to the given 'HEnv' to provide @random@ support.
--
-- @since 0.2.0.0
usingTxGen
  :: (TxGen `HEnv.NotElem` xs)
  => StdGen -> HEnv xs -> (HEnv (TxGen ': xs) -> IO a) -> IO a
usingTxGen g henv action = do
  withTxGen g \txGen -> action $ txGen `HEnv.Cons` henv

-- | Adds a new 'TxGen' to the given 'HEnv' to provide @random@ support.
--
-- @since 0.2.0.0
usingNewTxGen
  :: (TxGen `HEnv.NotElem` xs)
  => HEnv xs -> (HEnv (TxGen ': xs) -> IO a) -> IO a
usingNewTxGen henv action = do
  withNewTxGen $ \txGen -> action $ txGen `HEnv.Cons` henv

-- | Create a 'TxGen' from an existing 'StdGen'.
-- Inspired by 'System.Random.getStdRandom'.
--
-- @since 0.2.0.0
withTxGen :: StdGen -> (TxGen -> IO a) -> IO a
withTxGen g action = do
  genRef <- newIORef g
  action $ TxGen \f -> unsafeRunIOInTxM $ atomicModifyIORef' genRef (swap . f)

-- | Create a new 'TxGen'.
-- Inspired by 'System.Random.getStdRandom'.
--
-- @since 0.2.0.0
withNewTxGen :: (TxGen -> IO a) -> IO a
withNewTxGen action = do
  g <- newStdGen
  withTxGen g action

-- | Orphan instance for running 'MonadRandom' functions in 'TxM'.
-- Inspired by 'Control.Monad.Trans.Random.Strict.RandT'.
--
-- @since 0.2.0.0
instance (RandomEnv r) => MonadRandom (TxM r) where
  getRandomR lohi = getTxRandom $ randomR lohi
  getRandom = getTxRandom random
  getRandomRs lohi = getTxRandom (first (randomRs lohi) . split)
  getRandoms = getTxRandom (first randoms . split)
