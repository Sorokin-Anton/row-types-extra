{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Example where

import Data.Row.WebRecords
import Servant
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import Data.Swagger
import Servant.Swagger (HasSwagger(toSwagger))
import Network.Wai.Handler.Warp (run)
import Data.Generics.Labels ()
import Data.Aeson
import Control.Lens ((^..))


newtype UserID = UserID Int deriving newtype
  (ToJSON, FromJSON, ToSchema)

type User = Rec ("id" .== UserID .+ "username" .== String .+ "classes" .== [Class])
type Class = Rec ("grade" .== Int .+ "letter" .== Maybe Char .+ "school" .== Int)

bob :: User
bob = #id .== UserID 1
   .+ #username .== "Bob1337"
   .+ #classes .== [
        #grade .== 7
     .+ #letter .== Just 'm'
     .+ #school .== 57,
        #grade .== 8
     .+ #letter .== Just 'a'
     .+ #school .== 179
    ]

t1 :: [Maybe Char]
t1 = map (.letter) bob.classes

t2 :: [Maybe Char]
t2 = bob ^.. #classes . traverse . #letter

-- >>> t1 == t2
-- True

-- space-sensetive record-dot-preprocessor powered update
bib :: User
bib = bob{username = "bib"}

type API = "swagger.json" :> Get '[JSON] Swagger
                        :<|>  SwaggerSchemaUI "docs" "swagger.json"
                        :<|> AppAPI

type AppAPI = Get '[JSON] User

appSwagger :: Swagger
appSwagger = toSwagger (Proxy @AppAPI)

server :: Server API
server =
         pure appSwagger
    :<|> swaggerSchemaUIServer appSwagger
    :<|> pure bob

main :: IO ()
main = run 8080 $ serve (Proxy @API) server
