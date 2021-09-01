{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Data.Row.Records.CustomRec where

import           Data.Kind
import           Data.Row
import           Data.Row.Extra.Utils (type (++))
import qualified Deriving.Aeson       as AesonD
import           GHC.Generics         (Generic)
import           GHC.TypeLits         (Symbol)

data RecOption = AesonOption Type | SpecifiedRecName Symbol | Other Type

-- | We can add some `RecOption`s to our record for specifying instances
newtype CustomRec (mods :: [RecOption]) (row :: Row Type) = CustomRec (Rec row)
  deriving stock Generic


type family GetAesonOptions (xs :: [RecOption]) :: [Type] where
  GetAesonOptions '[] = '[]
  GetAesonOptions ('AesonOption x ': xs) = x :  GetAesonOptions xs
  GetAesonOptions (_ ': xs) = GetAesonOptions xs


infixr 0 #
-- | Operator to add custom parameters to existing rec type
type family (a :: [RecOption]) # (b :: Type) :: Type where
  opts # Rec row = CustomRec opts row
  opts # CustomRec oldOpts row = CustomRec (opts ++ oldOpts) row



type SnakeJSONed =  '[ 'AesonOption (AesonD.FieldLabelModifier AesonD.CamelToSnake) ]

type OmitNothingFields = '[ 'AesonOption AesonD.OmitNothingFields]
