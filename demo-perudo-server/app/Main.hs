{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Main where
-- Example how to build a servant server with interactive documentation  with help of
--   row-types, lens, hasql-th and record dot preprocessor.
-- This is naive backend for a perudo game

import Servant
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import Data.Swagger
import Servant.Swagger (HasSwagger(toSwagger))
import Network.Wai.Handler.Warp (run)
import Data.Generics.Labels ()
import Types (GameRoomsList)
import Data.Row.Records.CustomRec ( DefaultRecOptions )

type instance DefaultRecOptions = '[]

type API = "swagger.json" :> Get '[JSON] Swagger
                        :<|>  SwaggerSchemaUI "docs" "swagger.json"
                        :<|> AppAPI

type AppAPI = "gamerooms" :> Get '[JSON] GameRoomsList

appSwagger :: Swagger
appSwagger = toSwagger (Proxy @AppAPI)

server :: Server API
server =
         pure appSwagger
    :<|> swaggerSchemaUIServer appSwagger
    :<|> undefined



main :: IO ()
main = do
  putStrLn "Server started, you can interact with it at http://localhost:8080/docs"
  run 8080 $ serve (Proxy @API) server
