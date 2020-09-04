{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Example.Squeal.Internal.Queries where

import Data.Int (Int32)
import Database.PostgreSQL.Tx.Squeal
import Example.Squeal.Internal.Schema (Schemas)
import qualified Control.Exception as Exception
import qualified Example.Squeal.Internal.DB as DB

new :: IO DB.Handle
new =
  pure DB.Handle
    { DB.insertThreeMessages
    , DB.fetchThreeMessages

    , DB.close = mempty
    }

withHandle :: (DB.Handle -> IO a) -> IO a
withHandle = Exception.bracket new DB.close

insertThreeMessages
  :: String -> String -> String -> SquealM (Int, Int, Int)
insertThreeMessages s1 s2 s3 = fromSquealTxM do
  go >>= \case
    [k1, k2, k3] -> pure (k1, k2, k3)
    rows -> error $ "Expected exactly 3 rows, got " <> show (length rows)
  where
  go = executeParams stmt (s1, s2, s3) >>= getRows

  stmt :: Statement Schemas (String, String, String) Int
  stmt = Manipulation genericParams rowDecoder insertIntoFoo

  rowDecoder = fmap (fromIntegral :: Int32 -> Int) #id

  insertIntoFoo =
    insertInto #foo
      (Values
        ( Default `as` #id :* Set (param @1) `as` #message )
        [ Default `as` #id :* Set (param @2) `as` #message
        , Default `as` #id :* Set (param @3) `as` #message
        ])
      OnConflictDoRaise
      (Returning_ #id)

fetchThreeMessages
  :: Int -> Int -> Int -> SquealM (Maybe String, Maybe String, Maybe String)
fetchThreeMessages k1 k2 k3 = fromSquealTxM do
  rows <- go
  pure
    ( lookup k1 rows
    , lookup k2 rows
    , lookup k3 rows
    )
  where
  go = executeParams stmt params >>= getRows

  params :: (Int32, Int32, Int32)
  params =
    ( fromIntegral k1
    , fromIntegral k2
    , fromIntegral k3
    )

  stmt :: Statement Schemas (Int32, Int32, Int32) ((Int, String))
  stmt = Query paramEncoder rowDecoder selectFromFoo

  paramEncoder
    :: EncodeParams
        Schemas
        '[ 'NotNull 'PGint4, 'NotNull 'PGint4, 'NotNull 'PGint4 ]
        (Int32, Int32, Int32)
  paramEncoder = genericParams

  rowDecoder =
    (,)
      <$> fmap (fromIntegral :: Int32 -> Int) #id
      <*> #message

  selectFromFoo =
    select_
      (#id :* #message)
      (from (table #foo)
        & where_ (
            #id .== (param @1)
              .|| #id .== (param @2)
              .|| #id .== (param @3)))
