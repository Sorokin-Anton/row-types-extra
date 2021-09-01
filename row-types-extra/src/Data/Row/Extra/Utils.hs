{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Row.Extra.Utils where

import Data.Row.Internal
import Data.Row.Records
import Unsafe.Coerce (unsafeCoerce)

type family (a :: [k]) ++ (b :: [k]) :: [k] where
  '[] ++ a  = a
  (x ': xs) ++ a = x ': (xs ++ a)



frontExtend :: FrontExtends l t ('R r) => Rec ('R '[ l ':-> t] .+ 'R r) -> Rec ('R ((l ':-> t) : r))
frontExtend = unsafeCoerce
