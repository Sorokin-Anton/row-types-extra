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

module Data.Row.WebRecords (
  module Data.Row,
  module Data.Row.Records,
  MultiLabel(label)
  ) where
import Data.Row
-- import Data.Row.Records
import Data.Row.Aeson ()
import Data.Row.Instances.Swagger ()
import Data.Row.Instances.Arbitrary ()
import Data.Generics.Labels ()
import GHC.Records.Extra (HasField(..))
import Data.Row.Records (update, rename, restrict)
import Data.Row.Tuples

concat <$> mapM mkMultilabelTupleInstance [2..25]

instance (a ~ (r .! name), KnownSymbol name) => HasField name (Rec r) a where
  hasField r = (set, get) where
    set a = update (Label @name) a r
    get = r .! Label @name


-- >>> :t label (#a, #b)
-- label (#a, #b)
--   :: (tupleElem1, tupleElem2)
--      -> Rec ('R '[ "a" ':-> tupleElem1, "b" ':-> tupleElem2])

-- t :: Rec ("a" .== Char .+ "b" .== Bool)

-- >>> label (#a, #b, #c) ('x', True, 12)
-- #a .== 'x' .+ #b .== True .+ #c .== 12
