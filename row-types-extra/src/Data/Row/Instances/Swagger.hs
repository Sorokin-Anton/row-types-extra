{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE DataKinds #-}
module Data.Row.Instances.Swagger where

import Data.Row
import Data.Swagger
import GHC.Generics (Generic (Rep))
import Data.Swagger.Internal.Schema (GToSchema, named)
import Data.Swagger.Internal.TypeShape ( TypeHasSimpleShape )
import Data.Typeable (Typeable, typeRep, typeRepFingerprint)
import Data.Text (pack)

instance (Typeable a
  , Generic (Rec a)
  , GToSchema (Rep (Rec a))
  , TypeHasSimpleShape (Rec a) "genericDeclareSchemaUnrestricted")
  => ToSchema (Rec a)
  where
  declareNamedSchema proxy =
    fmap (named $ "Open product <" <> pack (show $ typeRepFingerprint $ typeRep proxy) <> ">")
   . genericDeclareSchema defaultSchemaOptions $ proxy
