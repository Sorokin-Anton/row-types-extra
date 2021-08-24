{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module GameLogic where

import Data.Row.Extra

newtype CubesInBid = CubesInBid Int
newtype CubeValue = CubeValue Int

type Bid = Rec ("amount" .== CubesInBid .+ "power" .== CubeValue)
