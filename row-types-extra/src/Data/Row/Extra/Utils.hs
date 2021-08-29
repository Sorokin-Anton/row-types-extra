{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Row.Extra.Utils where

type family (a :: [k]) ++ (b :: [k]) :: [k] where
  '[] ++ a  = a
  (x ': xs) ++ a = x ': (xs ++ a)
