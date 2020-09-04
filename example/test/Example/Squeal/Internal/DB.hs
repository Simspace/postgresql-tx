{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Example.Squeal.Internal.DB where

data Handle f = Handle
  { insertThreeMessages
      :: String -> String -> String -> f (Int, Int, Int)
  , fetchThreeMessages
      :: Int -> Int -> Int -> f (Maybe String, Maybe String, Maybe String)

  , close :: IO ()
  }
