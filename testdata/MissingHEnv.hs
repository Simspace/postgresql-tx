{-# LANGUAGE DataKinds #-}
module MissingHEnv where

import Database.PostgreSQL.Tx.HEnv

testMissingEnv :: Int
testMissingEnv = select h
  where
  h :: HEnv '[Char, Bool]
  h = fromGeneric ('a', True)
