{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import Data.Row
import Data.UUID (UUID)
import GameLogic (PlayerLogin)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Swagger (ToSchema)

-- We use newtypes to avoid, for example, messing `.id` fields of different things

newtype GameID = GameID UUID
   deriving stock Generic
   deriving anyclass (ToJSON, FromJSON, ToSchema)


data Color = Red | Green | Yellow | Blue | Magenta | Cyan | White | Black
   deriving stock (Generic, Show, Eq, Ord)
   deriving anyclass (ToJSON, FromJSON, ToSchema)


data GameStatus = WaitingStart | InProcess | GameFinished
   deriving stock (Generic, Show, Eq, Ord)
   deriving anyclass (ToJSON, FromJSON, ToSchema)

type GameRoomInfo = Rec
    ( "id" .== GameID
   .+ "state" .== GameStatus
   .+ "users" .== [ Rec
             ( "login" .== PlayerLogin
            .+ "color" .== Color
             )
                  ]

  .+ "seats" .== Rec
                (
        "free" .== Int
     .+ "total" .== Int
                )
    )

type GameRoomsList = Rec
  (
    "count" .== Int
 .+ "games" .== [
    GameRoomInfo
 ]

  )

-- >>> :kind! GameList
-- GameList :: *
-- = Rec
--     ('R
--        '[ "count" ':-> Int,
--           "games"
--           ':-> [Rec
--                   ('R
--                      '[ "id" ':-> GameID,
--                         "seats" ':-> Rec ('R '[ "free" ':-> Int, "total" ':-> Int]),
--                         "state" ':-> GameStatus,
--                         "users"
--                         ':-> [Rec ('R '[ "color" ':-> Color, "name" ':-> PlayerLogin])]])]])
