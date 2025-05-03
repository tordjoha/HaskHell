module Game (gameLoop) where

import Control.Monad.State
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Map as Map

import Types (GameState(..), Scene(..), MenuOption(..), Key(..), GameMonad)
import Menu (selectMenuOption, navigateMenuUp, navigateMenuDown)
import Player (moveLeft, moveRight)
import Enemy (moveEnemyTowards)
import Colors (armyGreen, groundColor)
import Rendering (drawElapsedTime, renderState)

initialGameState :: GameState
initialGameState = GameState
    { --currentScene = MenuScene Play -- ADD THIS BACK FOR SHOWING MENU. have to FIX keyboard input
      currentScene = PlayScene
    , playerScore = 0
    , elapsedTime = 0
    , playerPosition = (0, -310)
    , enemyPositions = []
    , keyStates = []
    , assets = Map.empty
    }

handleGameInput :: Event -> GameMonad ()
handleGameInput (EventKey (SpecialKey key) Down _ _) = do
    state <- get
    modify $ \s -> s { keyStates = key : keyStates s }
handleGameInput (EventKey (SpecialKey key) Up _ _) = do
    state <- get
    modify $ \s -> s { keyStates = filter (/= key) (keyStates s) }
handleGameInput _ = return ()

updateGameState :: Float -> GameState -> GameState
updateGameState dt state@GameState{currentScene = PlayScene, enemyPositions = enemies, playerPosition = playerPos@(px, py)} =
    let updatedState = foldr applyMovement state { elapsedTime = elapsedTime state + dt } (keyStates state)
        spawnInterval = 5  -- seconds
        newElapsed = elapsedTime updatedState
        --spawnPosition = (px + 300, py)  -- Spawn enemy 300 units to the right of player
        spawnPosition = (900, -310)
        newEnemies = if floor newElapsed `mod` spawnInterval == 0
                     then enemies ++ [spawnPosition]  -- Add new enemy at spawn position
                     else enemies
        movedEnemies = map (moveEnemyTowards playerPos) newEnemies
    in updatedState { enemyPositions = movedEnemies }
  where
    applyMovement :: SpecialKey -> GameState -> GameState
    applyMovement KeyLeft = moveLeft
    applyMovement KeyRight = moveRight
    applyMovement _ = id
updateGameState _ state = state  -- No changes for other scenes

gameLoop :: IO ()
gameLoop = do
    let screenHeight = 1080
    let assetFiles = [("monster", "./assets/baron.bmp"), ("player", "./assets/player.bmp")]
    assets <- mapM (\(name, path) -> fmap (name,) (loadBMP path)) assetFiles
    let imageMap = Map.fromList assets

    play FullScreen 
        black -- background
        60 -- FPS
        initialGameState { assets = imageMap}
        (renderState screenHeight) -- Render state with screen height
        (\e s -> execState (handleGameInput e) s) -- Handle input
        updateGameState -- Updates state per frame
