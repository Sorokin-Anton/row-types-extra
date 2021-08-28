{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}

module Data.Row.Extra (
  module Data.Row,
  module Data.Row.Records,
  module Data.Row.Extra,
  module Data.Row.GetRow,
  module Data.Row.Aeson.Custom,
  MultiLabel(label)
  ) where
import Data.Row
-- import Data.Row.Records
import Data.Row.Aeson.Custom
import Data.Row.Instances.Swagger ()
import Data.Row.Instances.Arbitrary ()
import Data.Generics.Labels ()
import GHC.Records.Extra (HasField(..))
import Data.Row.Records (update, rename, restrict, NativeRow, fromNative, FromNative)
import Data.Row.Tuples
import Data.Kind (Type)
import Data.Row.GetRow
import GHC.TypeLits (Symbol)

concat <$> mapM mkMultilabelTupleInstance [2..25]

instance (a ~ (r .! name), KnownSymbol name) => HasField name (Rec r) a where
  hasField r = (set, get) where
    set a = update (Label @name) a r
    get = r .! Label @name

-- >>> :t (.-)
-- (.-) :: KnownSymbol l => Rec r -> Label l -> Rec (r .- l)



infixl 6 <+>, <+$>, <$+>, <$+$>
type (a :: Type) <+> (b :: Type) = Rec (RowType a .+ RowType b)

(<+>) :: (GetRow a, GetRow b) =>
  a -> b -> a <+> b
a <+> b = getRow a .+ getRow b

(<+$>) :: (Functor f, GetRow a, GetRow b) => a -> f b -> f (a <+> b)
a <+$> b = (a <+>) <$> b

(<$+>) :: (Functor f, GetRow a, GetRow b) => f a -> b -> f (a <+> b)
a <$+> b = (<+> b) <$> a

(<$+$>) :: (Applicative f, GetRow a, GetRow b) =>
  f a -> f b -> f (a <+> b)
a <$+$> b = (<+>) <$> a <*> b

-- | Singleton record, can be used with `<+>` to extend other record
infixl 7 .=
type (a :: Symbol) .= (b :: Type) = Rec (a .== b)

(.=) :: KnownSymbol l => Label l -> a -> l .= a
a .= b = a .== b


-- | Add a label to "dirty" value to get a "dirty" record, useful with `<+$>`
($=) :: (Functor f, KnownSymbol l) => Label l -> f a -> f (l .= a)
a $= b = (a .==) <$> b

-- >>> :t label (#a, #b)
-- label (#a, #b)
--   :: (tupleElem1, tupleElem2)
--      -> Rec ('R '[ "a" ':-> tupleElem1, "b" ':-> tupleElem2])

-- t :: Rec ("a" .== Char .+ "b" .== Bool)

-- >>> label (#a, #b, #c) ('x', True, 12)
-- #a .== 'x' .+ #b .== True .+ #c .== 12
