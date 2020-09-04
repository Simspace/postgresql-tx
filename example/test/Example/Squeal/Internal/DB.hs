{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Example.Squeal.Internal.DB where

import Database.PostgreSQL.Tx.Squeal (SquealM)
import Example.Squeal.Internal.Schema (Schemas)

type M a = SquealM Schemas Schemas a

data Handle = Handle
  { insertThreeMessages
      :: String -> String -> String -> M (Int, Int, Int)
  , fetchThreeMessages
      :: Int -> Int -> Int -> M (Maybe String, Maybe String, Maybe String)

  , close :: IO ()
  }
