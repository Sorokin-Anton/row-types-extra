{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveAnyClass #-}

module GameLogic where


import Data.Row.Extra
import Data.Text (Text)
import GHC.Generics (Generic)
import Control.Monad.Except
import System.Random
import Control.Lens ((%~), only, (&), filteredBy, Traversal', (<%~), (<<>~))
import Data.Aeson (ToJSON, FromJSON)
import Data.Swagger (ToSchema)

data GameError = NoBidToCheck | NotYourTurn | IncorrectRaise | InvalidBid
  deriving stock (Generic, Show, Eq, Ord)

newtype CubeAmount = CubeAmount Int
  deriving stock Generic
  deriving newtype (Show, Eq, Ord, Num)

newtype CubeValue = CubeValue Int
  deriving stock Generic
  deriving newtype (Show, Eq, Ord)

newtype PlayerLogin = PlayerLogin Text
  deriving stock Generic
  deriving newtype (Show, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)


data PlayerAction = PlaceBid Bid | CheckBid | CheckEquals
  deriving stock (Show, Eq, Ord)

data CubeAction = AddCube | TakeCube
  deriving stock (Show, Eq, Ord)

data RoundType = Maputo | StandartRound
  deriving stock (Show, Eq, Ord)


type CheckResult = "player" .= PlayerLogin
             <+> "action" .= CubeAction

type InternalGameState = "players" .= ["login" .= PlayerLogin
                                   <+> "cubes" .= [CubeValue]
                                   <+> "hasMaputo" .= Bool]
                     <+> "bidHistory" .= [Bid]
                     <+> "turnOrder" .= [PlayerLogin]
                     <+> "roundType" .= RoundType

initGame :: MonadIO m => [PlayerLogin] -> m InternalGameState
initGame playerLogins =
        #roundType .= StandartRound
    <+> #bidHistory .= []
    <+> #turnOrder .= playerLogins
    <+$> #players $= forM playerLogins (\p ->
                   #login .= p
              <+>  #hasMaputo .= True
              <+$> #cubes $= rollCubes 5)

rollCubes :: MonadIO m => Int -> m [CubeValue]
rollCubes n = replicateM n (CubeValue <$> randomRIO (1,6))

-- monadic update of all elements of nested field, very nice IMHO
reroll :: MonadIO m => InternalGameState -> m InternalGameState
reroll = #players . traverse . #cubes $ rollCubes . length

-- >>> :i Traversal
-- Not in scope: ‘Traversal’

-- >>> :kind! InternalGameState
-- InternalGameState :: *
-- = Rec
--     ('R
--        '[ "aa" ':-> PlayerLogin,
--           "bid_history"
--           ':-> [Rec
--                   ('R
--                      '[ "amount" ':-> CubeAmount, "player" ':-> PlayerLogin,
--                         "value" ':-> CubeValue])],
--           "players"
--           ':-> [Rec
--                   ('R '[ "cubes" ':-> [CubeValue], "login" ':-> PlayerLogin])]])

type Bid = Rec ("player" .== PlayerLogin
             .+ "amount" .== CubeAmount
             .+ "value" .== CubeValue)



goodRaise :: InternalGameState -> Bid -> Bool
goodRaise game bid | game.roundType == Maputo = case game.bidHistory of
                    [] -> True
                    lastBid : _previous -> bid.value == lastBid.value && bid.amount > lastBid.amount

                | otherwise = case game.bidHistory of
                      [] -> bid.value /= CubeValue 1

                      lastBid : _previous -> case bid.value of
                        CubeValue 1 -> all (< bid.amount) [b.amount | b <- game.bidHistory, b.value == CubeValue 1]
                                       && (lastBid.value == CubeValue 1 || 2 * bid.amount >= lastBid.amount)
                        n -> case lastBid.value of
                          CubeValue 1 -> all (< bid.amount) [b.amount | b <- game.bidHistory]
                                      && bid.amount >= 2 * lastBid.amount
                          m -> bid.amount > lastBid.amount
                              || (bid.amount == lastBid.amount && n > m)

checkBid :: MonadError GameError m => InternalGameState -> PlayerLogin -> m CheckResult
checkBid game checkingUser = case game.bidHistory of
  [] -> throwError NoBidToCheck
  (bid : _old) -> do
    let
      -- allCubes = game ^.. #players . traverse . #cubes . traverse -- lens equivalent, longer
      -- allCubes = concatMap (.cubes) game.players
      allCubes = concat [p.cubes | p <- game.players]

      -- CubeValue 1 is a Joker at standart rounds
      goodCube c = (c == CubeValue 1 && game.roundType == StandartRound) || c == bid.value

    pure $ #action .= TakeCube <+> #player .=
      if bid.amount < CubeAmount (length $ filter goodCube allCubes)
      then  bid.player
      else checkingUser


checkEquals :: MonadError GameError m => InternalGameState -> PlayerLogin -> m CheckResult
checkEquals game checkingUser = case game.bidHistory of
  [] -> throwError NoBidToCheck
  (bid : _old) -> do
    let
      -- allCubes = game ^.. #players . traverse . #cubes . traverse -- lens equivalent, longer
      allCubes = concatMap (.cubes) game.players

      -- CubeValue 1 is a Joker at standart rounds
      goodCube c = (c == CubeValue 1 && game.roundType == StandartRound) || c == bid.value

    pure $ #player .= checkingUser <+> #action .=
      if bid.amount == CubeAmount (length $ filter goodCube allCubes)
      then AddCube
      else TakeCube

-- FIXME handle winning
-- FIXME REROLL!
gameStep :: (MonadError GameError m, MonadIO m) => PlayerAction -> PlayerLogin
  -> InternalGameState -> m InternalGameState
gameStep action player game = reroll . nextTurn =<< do
  unless (player == head game.turnOrder) $ throwError NotYourTurn
  case action of
    PlaceBid bid | bid.amount < CubeAmount 0
                 || bid.value < CubeValue 1
                 || bid.value > CubeValue 6
                   -> throwError InvalidBid
                 | otherwise -> undefined

    check -> do
      checkResult <- if check == CheckEquals then checkEquals game player else checkBid game player
      case checkResult.action of
        AddCube -> do
            let (cubes, updated) = game & atPlayer player . #cubes <<>~
                                   [CubeValue 1]
            liftIO . putStrLn $ "Player "
                                <> show player
                                <> " gained a cube and now has "
                                <> show (length cubes)
                                <> " cubes."
            return updated
        TakeCube -> do
          let (cubes, _updated) = game & atPlayer player . #cubes <%~
                      (<> [CubeValue 1])
          case cubes of
            _ -> undefined

nextTurn :: InternalGameState -> InternalGameState
nextTurn = #turnOrder %~ \(x  : xs) -> xs ++ [x]

atPlayer :: PlayerLogin -> Traversal' InternalGameState
                                  ("login" .= PlayerLogin
                                   <+> "cubes" .= [CubeValue]
                                   <+> "hasMaputo" .= Bool)
atPlayer player = #players . traverse . filteredBy (#login . only player)
