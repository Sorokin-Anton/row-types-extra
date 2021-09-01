{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Row.GetRow
  (GetRow(..), RowType) where


import Data.Row
import Data.Row.Records (NativeRow, fromNative, FromNative)
import Data.Kind (Type)
import Data.Row.Dictionaries (Unconstrained1)
import Data.Row.Records.CustomRec

type family RowType (a :: *) :: Row Type where
  RowType (Rec a) = a
  RowType (CustomRec mods a) = a
  RowType e = NativeRow e

data GetRowStrategy = SimpleExtractRow | WithGeneric

type family RowStrategy a :: GetRowStrategy where
  RowStrategy (Rec a) = 'SimpleExtractRow
  RowStrategy (CustomRec mods a) = 'SimpleExtractRow
  RowStrategy _ = 'WithGeneric

-- Without helper class, instance `... => GetRow a` will not allow us to write
-- different instances for GetRow (see Instance body), so we use Multiparamtypeclass trick
-- which allows parametrize different instances with type families
class strat ~ RowStrategy a => GetRow' a (strat :: GetRowStrategy) where
  getRow' :: a -> Rec (RowType a)

instance GetRow' (CustomRec mods a) 'SimpleExtractRow where
  getRow' (CustomRec a) = a

instance GetRow' (Rec a) 'SimpleExtractRow where
  getRow' = id

instance (RowStrategy a ~ 'WithGeneric, FromNative a, RowType a ~ NativeRow a)
  => GetRow' a 'WithGeneric where
  getRow' = fromNative


class Forall (RowType a) Unconstrained1 => GetRow a where
  getRow :: a -> Rec (RowType a)

instance (GetRow' a s, Forall (RowType a) Unconstrained1) => GetRow a where
  getRow = getRow'
