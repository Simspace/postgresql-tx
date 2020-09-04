{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Example.Squeal.Internal.DB where

import Database.PostgreSQL.Tx.Squeal (SquealM)

data Handle = Handle
  { insertThreeMessages
      :: String -> String -> String -> SquealM (Int, Int, Int)
  , fetchThreeMessages
      :: Int -> Int -> Int -> SquealM (Maybe String, Maybe String, Maybe String)

  , close :: IO ()
  }
