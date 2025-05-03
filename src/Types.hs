module Types (GameState(..), Scene(..), MenuOption(..), Key(..),  GameMonad) where

import Control.Monad.State (State)
import Graphics.Gloss (Picture)
import Graphics.Gloss.Interface.IO.Game (SpecialKey)
import qualified Data.Map as Map

data Key = Special SpecialKey | Char Char
    deriving (Show, Eq)

data GameState = GameState
    { currentScene :: Scene
    , playerScore :: Int
    , elapsedTime :: Float
    , playerPosition :: (Float, Float)
    , enemyPositions :: [(Float, Float)]
    , keyStates :: [SpecialKey]
    , assets :: Map.Map String Picture -- Stores all image assets
    , projectiles :: [(Float, Float)] -- Stores projectile positions
    } deriving Show

data Scene
    = MenuScene MenuOption
    | PlayScene
    | PauseScene
    | GameOverScene
    deriving (Show, Eq)

data MenuOption = Play | Exit
    deriving (Show, Eq)

type GameMonad a = State GameState a
