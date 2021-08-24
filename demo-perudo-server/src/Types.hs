{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Row
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Row.Extra

-- Some newtypes to avoid, for example, misuse of `id` field

newtype GameID = GameID UUID

newtype UserName = UserName Text

data Color = Red | Green | Yellow | Blue | Magenta | Cyan | White | Black

data GameState = WaitingStart | InProcess | GameFinished

type GameBasicInfo = Rec
    ( "id" .== GameID
   .+ "state" .== GameState
   .+ "users" .== [ Rec
             ( "name" .== UserName
            .+ "color" .== Color
             )
                  ]
    )

type GameList = Rec
  (
    "count" .== Int
 .+ "games" .== [
    GameBasicInfo <+ ("seats" .== Rec
      (
        "free" .== Int
     .+ "total" .== Int
      ))
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
--                         "state" ':-> GameState,
--                         "users"
--                         ':-> [Rec ('R '[ "color" ':-> Color, "name" ':-> UserName])]])]])
