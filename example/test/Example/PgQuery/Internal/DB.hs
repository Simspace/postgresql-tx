{-# LANGUAGE RankNTypes #-}
module Example.PgQuery.Internal.DB where

data Handle f = Handle
  { insertTwoMessages
      :: String -> String -> f (Int, Int)
  , fetchTwoMessages
      :: Int -> Int -> f (Maybe String, Maybe String)

  , close :: IO ()
  }
