{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
module Data.Row.Instances.Arbitrary where

import           Test.QuickCheck

import  Data.Row
import Data.Row.Internal
import GHC.TypeLits (Symbol)
import Data.Kind (Type)
import Data.Row.Extra.Utils (frontExtend)


instance Arbitrary (Rec Empty)  where
  arbitrary = pure empty

instance forall (l :: Symbol) (t :: Type) (r :: [LT Type]).
  (Arbitrary (Rec ('R r)),
   Arbitrary t,
   KnownSymbol l,
   FrontExtends l t ('R r)) =>
  Arbitrary (Rec
  ('R
  (l ':-> t ': r)
  )
            )
  where
    arbitrary  = do
      u <- arbitrary @t
      v <- arbitrary @(Rec ('R r))
      return . frontExtend $ (Label @l) .== u .+ v
