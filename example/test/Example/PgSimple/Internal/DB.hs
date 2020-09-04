{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Example.PgSimple.Internal.DB where

data Handle f = Handle
  { insertMessage :: String -> f Int
  , fetchMessage :: Int -> f (Maybe String)

  , close :: IO ()
  }
