{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Row.GetRow
  (GetRow(..), RowType) where


import Data.Row
import Data.Row.Records (NativeRow, fromNative, FromNative)
import Data.Kind (Type)

type family RowType (a :: *) :: Row Type where
  RowType (Rec a) = a
  RowType e = NativeRow e

data GetRowStrategy = SimpleExtractRow | WithGeneric

type family RowStrategy a :: GetRowStrategy where
  RowStrategy (Rec a) = 'SimpleExtractRow
  RowStrategy _ = 'WithGeneric

class strat ~ RowStrategy a => GetRow' a (strat :: GetRowStrategy) where
  getRow' :: a -> Rec (RowType a)

instance GetRow' (Rec a) 'SimpleExtractRow where
  getRow' = id

instance (RowStrategy a ~ 'WithGeneric, FromNative a, RowType a ~ NativeRow a)
  => GetRow' a 'WithGeneric where
  getRow' = fromNative


class GetRow a where
  getRow :: a -> Rec (RowType a)

instance GetRow' a s => GetRow a where
  getRow = getRow'
