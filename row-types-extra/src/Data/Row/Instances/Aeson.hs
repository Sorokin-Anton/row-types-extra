{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE PolyKinds            #-}
module Data.Row.Instances.Aeson where

import           Data.Aeson
import           Data.Row
import           Data.Row.Records.CustomRec
import qualified Deriving.Aeson             as AesonD
import           GHC.Generics

instance (AesonD.AesonOptions (GetAesonOptions opts),
          Generic (Rec row), GToJSON' Value Zero (Rep (Rec row)))
          => ToJSON (CustomRec opts row) where
  toJSON (CustomRec r) = genericToJSON (AesonD.aesonOptions @(GetAesonOptions opts)) r

instance (AesonD.AesonOptions (GetAesonOptions opts),
          Generic (Rec row), GFromJSON Zero (Rep (Rec row)))
          => FromJSON (CustomRec opts row) where
  parseJSON = fmap CustomRec <$> genericParseJSON (AesonD.aesonOptions @(GetAesonOptions opts))



deriving via CustomRec '[] row instance (Generic (Rec row), GToJSON' Value Zero (Rep (Rec row)))
          => ToJSON (Rec row)

deriving via CustomRec '[] row instance (Generic (Rec row),  GFromJSON Zero (Rep (Rec row)))
          => FromJSON (Rec row)
