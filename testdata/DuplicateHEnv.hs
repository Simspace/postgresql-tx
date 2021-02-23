{-# LANGUAGE DataKinds #-}
module DuplicateHEnv where

import Database.PostgreSQL.Tx.HEnv

testDuplicateEnv :: Int
testDuplicateEnv = select h
  where
  h :: HEnv '[Int, Char, Bool, Int]
  h = fromGeneric (1, 'a', True, 2)
