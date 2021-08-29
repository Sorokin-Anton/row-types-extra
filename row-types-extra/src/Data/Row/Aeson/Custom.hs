{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE PolyKinds #-}
module Data.Row.Aeson.Custom where

import GHC.TypeLits (Symbol)
import Data.Kind (Type)
import Data.Row
import Data.Aeson
import qualified Deriving.Aeson as AesonD
import GHC.Generics
import Data.Row.Extra.Utils (type (++))


-- | We can add some `RecOption`s to our record for specifying instances
newtype CustomRec (mods :: [RecOption]) (row :: Row Type) = CustomRec (Rec row)
  deriving stock Generic

instance (AesonD.AesonOptions (GetAesonOptions opts),
          Generic (Rec row), GToJSON' Value Zero (Rep (Rec row)))
          => ToJSON (CustomRec opts row) where
  toJSON (CustomRec r) = genericToJSON (AesonD.aesonOptions @(GetAesonOptions opts)) r

instance (AesonD.AesonOptions (GetAesonOptions opts),
          Generic (Rec row), GFromJSON Zero (Rep (Rec row)))
          => FromJSON (CustomRec opts row) where
  parseJSON = fmap CustomRec <$> genericParseJSON (AesonD.aesonOptions @(GetAesonOptions opts))


data RecOption = AesonOption Type | SpecifiedRecName Symbol

type family GetAesonOptions (xs :: [RecOption]) :: [Type] where
  GetAesonOptions '[] = '[]
  GetAesonOptions ('AesonOption x ': xs) = x :  GetAesonOptions xs
  GetAesonOptions (_ ': xs) = GetAesonOptions xs

deriving via CustomRec '[] row instance (Generic (Rec row), GToJSON' Value Zero (Rep (Rec row)))
          => ToJSON (Rec row)

deriving via CustomRec '[] row instance (Generic (Rec row),  GFromJSON Zero (Rep (Rec row)))
          => FromJSON (Rec row)

infixr 0 #
-- add custom parameters to record
type family (a :: [RecOption]) # (b :: Type) :: Type where
  opts # Rec row = CustomRec opts row
  opts # CustomRec oldOpts row = CustomRec (opts ++ oldOpts) row



type SnakeJSONed =  '[ 'AesonOption (AesonD.FieldLabelModifier AesonD.CamelToSnake) ]

type OmitNothingFields = '[ 'AesonOption AesonD.OmitNothingFields]
