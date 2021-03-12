{-# LANGUAGE DataKinds #-}
module DuplicateHEnv where

import Database.PostgreSQL.Tx.HEnv

testDuplicateEnv :: Int
testDuplicateEnv = select h
  where
  h :: HEnv '[Int, Char, Bool, Int]
  h = fromGeneric (1, 'a', True, 2)

-- Still causes a compiler error even if we're not selecting that type.
testDuplicateEnv' :: Char
testDuplicateEnv' = select h
  where
  h :: HEnv '[Char, Int, Bool, Int]
  h = fromGeneric ('a', 1, True, 2)
