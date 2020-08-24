{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Example.PgSimple.Internal.DB where

import Database.PostgreSQL.Tx.Simple (PgSimpleM)

data Handle = Handle
  { insertMessage :: String -> PgSimpleM Int
  , fetchMessage :: Int -> PgSimpleM (Maybe String)

  , close :: IO ()
  }
