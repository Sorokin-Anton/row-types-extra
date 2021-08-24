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
  MultiLabel(label)
  ) where
import Data.Row
-- import Data.Row.Records
import Data.Row.Aeson ()
import Data.Row.Instances.Swagger ()
import Data.Row.Instances.Arbitrary ()
import Data.Generics.Labels ()
import GHC.Records.Extra (HasField(..))
import Data.Row.Records (update, rename, restrict, NativeRow, fromNative, FromNative)
import Data.Row.Tuples
import Data.Kind (Type)
import Data.Row.Dictionaries (Unconstrained1)
import Data.Row.GetRow

concat <$> mapM mkMultilabelTupleInstance [2..25]

instance (a ~ (r .! name), KnownSymbol name) => HasField name (Rec r) a where
  hasField r = (set, get) where
    set a = update (Label @name) a r
    get = r .! Label @name




infixl 6 <+
type  (a :: Type) <+ (r :: Row Type) = Rec (RowType a .+ r)

(<+) :: (GetRow a, Forall (RowType a) Unconstrained1) => a -> Rec r -> a <+ r
b <+ r = getRow b .+ r

type a +> b = b <+ a

(+>) :: (GetRow a, Forall (RowType a) Unconstrained1) =>
    Rec r -> a -> r +> a
a +> b = b <+ a

infixl 6 <+>

type (a :: Type) <+> (b :: Type) = Rec (RowType a .+ RowType b)

(<+>) :: (GetRow a, Forall (RowType a) Unconstrained1, GetRow b) =>
  a -> b -> a <+> b
a <+> b = getRow a .+ getRow b


type a .= b = Rec (a .== b)


-- >>> :t label (#a, #b)
-- label (#a, #b)
--   :: (tupleElem1, tupleElem2)
--      -> Rec ('R '[ "a" ':-> tupleElem1, "b" ':-> tupleElem2])

-- t :: Rec ("a" .== Char .+ "b" .== Bool)

-- >>> label (#a, #b, #c) ('x', True, 12)
-- #a .== 'x' .+ #b .== True .+ #c .== 12
