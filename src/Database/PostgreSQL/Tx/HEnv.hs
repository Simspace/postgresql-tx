{-# LANGUAGE ConstraintKinds #-}
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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Database.PostgreSQL.Tx.HEnv
  ( HEnv(Nil, Cons)
  , UniqueElem
  , select
  , singleton
  , fromGeneric
  , fromTuple
  ) where

import Data.Kind (Constraint)
import Database.PostgreSQL.Tx (TxEnv(lookupTxEnv))
import GHC.Generics
import GHC.TypeLits (ErrorMessage((:<>:), ShowType, Text), TypeError)

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

-- | 'TxEnv' instance for 'HEnv'; selects the @a@ in the 'HEnv'
-- and makes it available via the runtime environment.
-- If there are multiple @a@ values, it will be rejected
-- by the compiler due to 'UniqueElem'.
--
-- @since 0.2.0.0
instance (UniqueElem a xs) => TxEnv a (HEnv xs) where
  lookupTxEnv = select

select :: (UniqueElem a xs) => HEnv xs -> a
select = select'

-- | Constraint for asserting that @a@ exists uniquely in the type list @xs@.
-- Hides the 'Select' type class so it is closed from extension.
--
-- @since 0.4.0.0
type UniqueElem a xs =
  ( Select a xs
  , UniqueElemGo a xs xs 'False
  , AllUnique xs xs
  ) :: Constraint

type family AllUnique (xs :: [*]) (orig :: [*]) :: Constraint where
  AllUnique '[] orig = ()

  AllUnique (x ': xs) orig =
    ( NotDuplicated x xs orig
    , AllUnique xs orig
    )

type family NotDuplicated (x :: *) (ys :: [*]) (orig :: [*]) :: Constraint where
  NotDuplicated x '[] orig = ()
  NotDuplicated x (x ': xs) orig =
    TypeError
      ( 'ShowType x
          ':<>: 'Text " duplicated in HEnv "
          ':<>: 'ShowType orig
      )
  NotDuplicated x (y ': ys) orig = NotDuplicated x ys orig

-- | Constraint for proving that @a@ both exists in @xs@ and also is
-- not duplicated.
type family UniqueElemGo a (xs :: [*]) (orig :: [*]) (found :: Bool) :: Constraint where
  UniqueElemGo x '[] orig 'True = ()

  UniqueElemGo x '[] orig 'False =
    TypeError
      ( 'ShowType x
          ':<>: 'Text " expected in HEnv "
          ':<>: 'ShowType orig
      )

  UniqueElemGo x (x ': xs) orig 'False = UniqueElemGo x xs orig 'True

  UniqueElemGo x (x ': xs) orig 'True =
    TypeError
      ( 'ShowType x
          ':<>: 'Text " duplicated in HEnv "
          ':<>: 'ShowType orig
      )

  UniqueElemGo a (x ': xs) orig found = UniqueElemGo a xs orig found

-- | Internal type class for selecting the first @a@ in an 'HEnv'.
-- Type class is not exported; if you need this as a constraint, use 'Elem'.
class Select a xs where
  select' :: HEnv xs -> a

instance Select a (a ': xs) where
  select' (a `Cons` _) = a

instance {-# OVERLAPPABLE #-} (Select a xs) => Select a (x ': xs) where
  select' (_ `Cons` xs) = select' xs

-- | Internal type class for appending 'HEnv' values; useful for 'FromGeneric'.
class Append xs ys where
  type AppendList xs ys :: [*]
  append :: HEnv xs -> HEnv ys -> HEnv (AppendList xs ys)

instance Append '[] ys where
  type AppendList '[] ys = ys
  append _ ys = ys

instance (Append xs ys) => Append (x ': xs) ys where
  type AppendList (x ': xs) ys = x ': AppendList xs ys
  append (x `Cons` xs) ys = x `Cons` append xs ys

-- | Construct an 'HEnv' from a 'Generic' structure.
fromGeneric :: (Generic i, FromGeneric (Rep i) o) => i -> HEnv o
fromGeneric = fromGGeneric . from
{-# INLINE fromGeneric #-}

-- | Internal type class for constructing an 'HEnv' using 'Generic'.
class FromGeneric f o | f -> o where
  fromGGeneric :: f p -> HEnv o

instance FromGeneric U1 '[] where
  fromGGeneric _ = Nil
  {-# INLINE fromGGeneric #-}

instance (FromGeneric f o) => FromGeneric (M1 D x f) o where
  fromGGeneric (M1 x) = fromGGeneric x
  {-# INLINE fromGGeneric #-}

instance (FromGeneric f o) => FromGeneric (M1 C x f) o where
  fromGGeneric (M1 x) = fromGGeneric x
  {-# INLINE fromGGeneric #-}

instance FromGeneric (M1 S x (K1 R a)) '[a] where
  fromGGeneric (M1 (K1 a)) = singleton a
  {-# INLINE fromGGeneric #-}

instance
  ( FromGeneric i1 o1
  , FromGeneric i2 o2
  , Append o1 o2
  , o ~ AppendList o1 o2
  ) => FromGeneric (i1 :*: i2) o
  where
  fromGGeneric (f :*: g) = fromGGeneric f `append` fromGGeneric g
  {-# INLINE fromGGeneric #-}

-- | Internal type class for constructing an 'HEnv' from a tuple.
class FromTuple i o | i -> o where

  -- | Construct an 'HEnv' from the given tuple @i@.
  -- Instances support tuples of up to 16 elements.
  --
  -- @since 0.2.0.0
  fromTuple :: i -> HEnv o

instance FromTuple (x1, x2) '[x1, x2] where
  fromTuple (x1, x2) = x1 `Cons` x2 `Cons` Nil
  {-# INLINE fromTuple #-}
instance FromTuple (x1, x2, x3) '[x1, x2, x3] where
  fromTuple (x1, x2, x3) = x1 `Cons` x2 `Cons` x3 `Cons` Nil
  {-# INLINE fromTuple #-}
instance FromTuple (x1, x2, x3, x4) '[x1, x2, x3, x4] where
  fromTuple (x1, x2, x3, x4) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` Nil
  {-# INLINE fromTuple #-}
instance FromTuple (x1, x2, x3, x4, x5) '[x1, x2, x3, x4, x5] where
  fromTuple (x1, x2, x3, x4, x5) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` x5 `Cons` Nil
  {-# INLINE fromTuple #-}
instance FromTuple (x1, x2, x3, x4, x5, x6) '[x1, x2, x3, x4, x5, x6] where
  fromTuple (x1, x2, x3, x4, x5, x6) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` x5 `Cons` x6 `Cons` Nil
  {-# INLINE fromTuple #-}
instance FromTuple (x1, x2, x3, x4, x5, x6, x7) '[x1, x2, x3, x4, x5, x6, x7] where
  fromTuple (x1, x2, x3, x4, x5, x6, x7) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` x5 `Cons` x6 `Cons` x7 `Cons` Nil
  {-# INLINE fromTuple #-}
instance FromTuple (x1, x2, x3, x4, x5, x6, x7, x8) '[x1, x2, x3, x4, x5, x6, x7, x8] where
  fromTuple (x1, x2, x3, x4, x5, x6, x7, x8) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` x5 `Cons` x6 `Cons` x7 `Cons` x8 `Cons` Nil
  {-# INLINE fromTuple #-}
instance FromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9) '[x1, x2, x3, x4, x5, x6, x7, x8, x9] where
  fromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` x5 `Cons` x6 `Cons` x7 `Cons` x8 `Cons` x9 `Cons` Nil
  {-# INLINE fromTuple #-}
instance FromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10] where
  fromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` x5 `Cons` x6 `Cons` x7 `Cons` x8 `Cons` x9 `Cons` x10 `Cons` Nil
  {-# INLINE fromTuple #-}
instance FromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11] where
  fromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` x5 `Cons` x6 `Cons` x7 `Cons` x8 `Cons` x9 `Cons` x10 `Cons` x11 `Cons` Nil
  {-# INLINE fromTuple #-}
instance FromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12] where
  fromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` x5 `Cons` x6 `Cons` x7 `Cons` x8 `Cons` x9 `Cons` x10 `Cons` x11 `Cons` x12 `Cons` Nil
  {-# INLINE fromTuple #-}
instance FromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13] where
  fromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` x5 `Cons` x6 `Cons` x7 `Cons` x8 `Cons` x9 `Cons` x10 `Cons` x11 `Cons` x12 `Cons` x13 `Cons` Nil
  {-# INLINE fromTuple #-}
instance FromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14] where
  fromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` x5 `Cons` x6 `Cons` x7 `Cons` x8 `Cons` x9 `Cons` x10 `Cons` x11 `Cons` x12 `Cons` x13 `Cons` x14 `Cons` Nil
  {-# INLINE fromTuple #-}
instance FromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15] where
  fromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` x5 `Cons` x6 `Cons` x7 `Cons` x8 `Cons` x9 `Cons` x10 `Cons` x11 `Cons` x12 `Cons` x13 `Cons` x14 `Cons` x15 `Cons` Nil
  {-# INLINE fromTuple #-}
instance FromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16) '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16] where
  fromTuple (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16) = x1 `Cons` x2 `Cons` x3 `Cons` x4 `Cons` x5 `Cons` x6 `Cons` x7 `Cons` x8 `Cons` x9 `Cons` x10 `Cons` x11 `Cons` x12 `Cons` x13 `Cons` x14 `Cons` x15 `Cons` x16 `Cons` Nil
  {-# INLINE fromTuple #-}

{- Instances above generated with:
import Data.List
main = do
  flip mapM_ [2..16] $ \i -> do
    let args = flip map [1..i] $ \j -> "x" ++ show j
    let commaSep = intercalate ", " args
    putStrLn $ concat
      [ "instance FromTuple ("
      , commaSep
      , ") '["
      , commaSep
      , "] where fromTuple ("
      , commaSep
      , ") = "
      , intercalate " `Cons` " (args ++ ["Nil"])
      , " {-# INLINE fromTuple #-}"
      ]
-}
