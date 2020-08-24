{-# LANGUAGE RankNTypes #-}
module Example.PgQuery.Internal.DB where

import Database.PostgreSQL.Tx.Query (PgQueryM)

data Handle = Handle
  { insertTwoMessages
      :: String -> String -> PgQueryM (Int, Int)
  , fetchTwoMessages
      :: Int -> Int -> PgQueryM (Maybe String, Maybe String)

  , close :: IO ()
  }
