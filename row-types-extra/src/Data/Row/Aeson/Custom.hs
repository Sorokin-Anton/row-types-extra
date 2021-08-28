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
module Data.Row.Aeson.Custom where

import GHC.TypeLits (Symbol)
import Data.Kind (Type)
import Data.Row
import Data.Aeson
import Deriving.Aeson
import GHC.Generics


-- | We can add some `RecOption`s to our record for specifying instances
newtype CustomRec (mods :: [RecOption]) (row :: Row Type) = CustomRec (Rec row)
  deriving stock Generic

instance (AesonOptions (GetAesonOptions opts),
          Generic (Rec row), GToJSON' Value Zero (Rep (Rec row)))
          => ToJSON (CustomRec opts row) where
  toJSON (CustomRec r) = genericToJSON (aesonOptions @(GetAesonOptions opts)) r

instance (AesonOptions (GetAesonOptions opts),
          Generic (Rec row), GFromJSON Zero (Rep (Rec row)))
          => FromJSON (CustomRec opts row) where
  parseJSON = fmap CustomRec <$> genericParseJSON (aesonOptions @(GetAesonOptions opts))


data RecOption = AesonOption Type | SpecifiedRecName Symbol

type family GetAesonOptions (xs :: [RecOption]) :: [Type] where
  GetAesonOptions '[] = '[]
  GetAesonOptions ('AesonOption x ': xs) = x :  GetAesonOptions xs
  GetAesonOptions (_ ': xs) = GetAesonOptions xs

type Snake = '[ 'AesonOption (FieldLabelModifier CamelToSnake) ]

deriving via CustomRec '[] row instance (Generic (Rec row), GToJSON' Value Zero (Rep (Rec row)))
          => ToJSON (Rec row)

deriving via CustomRec '[] row instance (Generic (Rec row),  GFromJSON Zero (Rep (Rec row)))
          => FromJSON (Rec row)
