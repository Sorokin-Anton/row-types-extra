{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MonoLocalBinds #-}

module Data.Row.Instances.Swagger where

import Data.Row
import Data.Swagger
import Data.Swagger.Internal.Schema (named, GToSchema)
import Data.Typeable (Typeable, typeRep, typeRepFingerprint, Proxy (Proxy))
import Data.Text (pack)
import Data.Row.Records.CustomRec
import qualified Deriving.Aeson as AesonD
import GHC.Generics (Generic (Rep))
import Data.Swagger.Internal.TypeShape (TypeHasSimpleShape)

instance (Typeable a, Typeable opts
  , AesonD.AesonOptions (GetAesonOptions opts)
  , Generic (Rec a)
  , GToSchema (Rep (Rec a))
  , TypeHasSimpleShape (Rec a) "genericDeclareSchemaUnrestricted")
  => ToSchema (CustomRec opts a)
  where
  declareNamedSchema proxy =
    fmap (named $ "Open product <" <> pack (show $ typeRepFingerprint $ typeRep proxy) <> ">")
   . genericDeclareSchema (fromAesonOptions (AesonD.aesonOptions @(GetAesonOptions opts))) $ (Proxy :: Proxy (Rec a))

deriving via CustomRec DefaultRecOptions a instance
  (Typeable DefaultRecOptions, AesonD.AesonOptions (GetAesonOptions DefaultRecOptions), Typeable a, Generic (Rec a),
   GToSchema (Rep (Rec a)),
   TypeHasSimpleShape (Rec a) "genericDeclareSchemaUnrestricted")
   => ToSchema (Rec a)
