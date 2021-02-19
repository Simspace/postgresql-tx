{-# LANGUAGE BlockArguments #-}
module Main where

import Control.Monad (replicateM)
import Database.PostgreSQL.Tx.Random (getRandom, getRandomR, getRandomRs, getRandoms, randomRTxM, randomTxM, usingTxGen)
import Database.PostgreSQL.Tx.Unsafe (unsafeRunTxM)
import System.Random (mkStdGen)
import Test.Hspec (describe, hspec, it, shouldReturn)
import qualified Database.PostgreSQL.Tx.HEnv as HEnv

main :: IO ()
main = hspec do
  describe "random" do
    describe "specialized" do
      it "randomTxM" do
        runTxM (replicateM 3 randomTxM)
          `shouldReturn`
            [ -257916191140261834 :: Int
            , -3770489275815013535
            ,  1353841796473577247
            ]
      it "randomRTxM" do
        runTxM (replicateM 3 (randomRTxM (0, 10)))
          `shouldReturn` [9, 4, 8 :: Int]

    describe "MonadRandom" do
      it "getRandomR" do
        runTxM (replicateM 3 (getRandomR (0, 10)))
          `shouldReturn` [9, 4, 8 :: Int]
      it "getRandom" do
        runTxM (replicateM 3 getRandom)
          `shouldReturn`
            [ -257916191140261834 :: Int
            , -3770489275815013535
            ,  1353841796473577247
            ]
      it "getRandomRs" do
        runTxM (replicateM 3 (take 3 <$> getRandomRs (0, 10)))
          `shouldReturn`
            [ [ 3 , 4  , 5 :: Int ]
            , [ 8 , 10 , 0 ]
            , [ 8 , 0  , 9 ]
            ]
      it "getRandoms" do
        runTxM (replicateM 3 (take 3 <$> getRandoms))
          `shouldReturn`
            [ [  447840136503420170 :: Int
              , -8672555702697140668
              ,  5602589590243044930
              ]
            , [ -9058495492241094619
              , -7589256844121862470
              ,  8279389114702829405
              ]
            , [ -3252924011971007656
              , -1272934105980727847
              ,  4452370816793124485
              ]
            ]
  where
  myStdGen = mkStdGen 4

  runTxM action = usingTxGen myStdGen HEnv.Nil \henv -> unsafeRunTxM henv action
