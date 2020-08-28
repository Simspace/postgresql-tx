{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Database.PostgreSQL.Tx.HEnv
  ( HEnv(Nil, Cons)
  , singleton
  , fromTuple
  ) where

import Database.PostgreSQL.Tx (TxEnv(lookupTxEnv))

-- | Glorified hlist used to construct ad hoc @tx@ runtime environments.
data family HEnv (l :: [*])
data instance HEnv '[] = Nil
data instance HEnv (x ': xs) = x `Cons` HEnv xs
infixr 2 `Cons`

-- | Construct an 'HEnv' containing a single value.
--
-- @since 0.2.0.0
singleton :: a -> HEnv '[a]
singleton = (`Cons` Nil)

-- | 'TxEnv' instance for 'HEnv'; selects the first @a@ in the 'HEnv'
-- and makes it available via the runtime environment.
--
-- @since 0.2.0.0
instance (Select a xs) => TxEnv a (HEnv xs) where
  lookupTxEnv = select

-- | Internal type class for selecting the first @a@ in an 'HEnv'.
class Select a xs where
  select :: HEnv xs -> a

instance Select a (a ': xs) where
  select (a `Cons` _) = a

instance {-# OVERLAPPABLE #-} (Select a xs) => Select a (x ': xs) where
  select (_ `Cons` xs) = select xs

-- | Internal type class for constructing an 'HEnv' from a tuple.
class FromTuple i o | i -> o where

  -- | Construct an 'HEnv' from the given tuple @i@.
  -- Instances support tuples of up to 16 elements.
  --
  -- @since 0.2.0.0
  fromTuple :: i -> HEnv o

instance FromTuple (x1, x2) '[x1, x2] where fromTuple (x1, x2) = x1 `Cons` x2 `Cons` Nil
instance FromTuple (x1, x2, x3) '[x1, x2, x3] where fromTuple (x1, x2, x3) = x1 `Cons` x2 `Cons` x3 `Cons` Nil
instance FromTuple (x1, x2, x3, x4) '[x1, x2, x3, x4] where fromTuple (x1, x2, x3, x4) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` Nil
instance FromTuple (x1, x2, x3, x4, x5) '[x1, x2, x3, x4, x5] where fromTuple (x1, x2, x3, x4, x5) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` x5 `Cons` Nil
instance FromTuple (x1, x2, x3, x4, x5, x6) '[x1, x2, x3, x4, x5, x6] where fromTuple (x1, x2, x3, x4, x5, x6) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` x5 `Cons` x6 `Cons` Nil
instance FromTuple (x1, x2, x3, x4, x5, x6, x7) '[x1, x2, x3, x4, x5, x6, x7] where fromTuple (x1, x2, x3, x4, x5, x6, x7) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` x5 `Cons` x6 `Cons` x7 `Cons` Nil
instance FromTuple (x1, x2, x3, x4, x5, x6, x7, x8) '[x1, x2, x3, x4, x5, x6, x7, x8] where fromTuple (x1, x2, x3, x4, x5, x6, x7, x8) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` x5 `Cons` x6 `Cons` x7 `Cons` x8 `Cons` Nil
instance FromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9) '[x1, x2, x3, x4, x5, x6, x7, x8, x9] where fromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` x5 `Cons` x6 `Cons` x7 `Cons` x8 `Cons` x9 `Cons` Nil
instance FromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10] where fromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` x5 `Cons` x6 `Cons` x7 `Cons` x8 `Cons` x9 `Cons` x10 `Cons` Nil
instance FromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11] where fromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` x5 `Cons` x6 `Cons` x7 `Cons` x8 `Cons` x9 `Cons` x10 `Cons` x11 `Cons` Nil
instance FromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12] where fromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` x5 `Cons` x6 `Cons` x7 `Cons` x8 `Cons` x9 `Cons` x10 `Cons` x11 `Cons` x12 `Cons` Nil
instance FromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13] where fromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` x5 `Cons` x6 `Cons` x7 `Cons` x8 `Cons` x9 `Cons` x10 `Cons` x11 `Cons` x12 `Cons` x13 `Cons` Nil
instance FromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14] where fromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` x5 `Cons` x6 `Cons` x7 `Cons` x8 `Cons` x9 `Cons` x10 `Cons` x11 `Cons` x12 `Cons` x13 `Cons` x14 `Cons` Nil
instance FromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15] where fromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` x5 `Cons` x6 `Cons` x7 `Cons` x8 `Cons` x9 `Cons` x10 `Cons` x11 `Cons` x12 `Cons` x13 `Cons` x14 `Cons` x15 `Cons` Nil
instance FromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16) '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16] where fromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` x5 `Cons` x6 `Cons` x7 `Cons` x8 `Cons` x9 `Cons` x10 `Cons` x11 `Cons` x12 `Cons` x13 `Cons` x14 `Cons` x15 `Cons` x16 `Cons` Nil
{- Instances above generated with:
import Data.List
main = do
  flip mapM_ [2..16] $ \i -> do
    let args = flip map [1..i] $ \j -> "x" ++ show j
    let commaSep = intercalate ", " args
    putStrLn $ concat
      [ "instance Mk ("
      , commaSep
      , ") '["
      , commaSep
      , "] where mk ("
      , commaSep
      , ") = "
      , intercalate " `Cons` " (args ++ ["Nil"])
      ]
-}
