module Game (gameLoop) where

import Control.Monad.State
import Control.Monad (when)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Map as Map

import Types (GameState(..), Scene(..), MenuOption(..), Key(..), GameMonad)
import Menu (selectMenuOption, navigateMenuUp, navigateMenuDown)
import Player (moveLeft, moveRight, shootProjectile)
import Enemy (moveEnemyTowards)
import Colors (armyGreen, groundColor)
import Rendering (drawElapsedTime, renderState)

initialGameState :: GameState
initialGameState = GameState
    { currentScene = MenuScene Play -- ADD THIS BACK FOR SHOWING MENU
      --currentScene = PlayScene
    , playerScore = 0
    , elapsedTime = 0
    , playerPosition = (0, -290)
    , enemyPositions = []
    , keyStates = []
    , assets = Map.empty
    , projectiles = []
    }

handleGameInput :: Event -> GameMonad ()
handleGameInput (EventKey (SpecialKey KeyUp) Down _ _) = do
    state <- get
    when (currentScene state == MenuScene Play || currentScene state == MenuScene Exit) $
        modify navigateMenuUp
handleGameInput (EventKey (SpecialKey KeyDown) Down _ _) = do
    state <- get
    when (currentScene state == MenuScene Play || currentScene state == MenuScene Exit) $
        modify navigateMenuDown
handleGameInput (EventKey (SpecialKey KeyEnter) Down _ _) = do
    state <- get
    when (currentScene state == MenuScene Play || currentScene state == MenuScene Exit) $
        modify selectMenuOption
handleGameInput (EventKey (SpecialKey key) Down _ _) = do
    state <- get
    modify $ \s -> s { keyStates = key : keyStates s }
handleGameInput (EventKey (SpecialKey key) Up _ _) = do
    state <- get
    modify $ \s -> s { keyStates = filter (/= key) (keyStates s) }
handleGameInput _ = return ()

updateGameState :: Float -> GameState -> GameState
updateGameState dt state@GameState{currentScene = PlayScene, enemyPositions = enemies, playerPosition = playerPos@(px, py), projectiles = ps} =
    let 
        playerMovementState = foldr applyMovementKeys state { elapsedTime = elapsedTime state + dt } (keyStates state) 
        updatedState = if KeyTab `elem` keyStates state
                      then shootProjectile playerMovementState
                      else playerMovementState
        movedProjectiles = [(x + 10, y) | (x, y) <- projectiles updatedState]
        spawnInterval = 5
        newElapsed = elapsedTime updatedState
        spawnPosition = (900, -310)
        newEnemies = if floor newElapsed `mod` spawnInterval == 0
                     then enemies ++ [spawnPosition]
                     else enemies
        movedEnemies = map (moveEnemyTowards playerPos) newEnemies
    in updatedState { enemyPositions = movedEnemies, projectiles = movedProjectiles }
  where
    applyMovementKeys :: SpecialKey -> GameState -> GameState
    applyMovementKeys KeyLeft = moveLeft
    applyMovementKeys KeyRight = moveRight
    applyMovementKeys _ = id
updateGameState _ state = state  -- No changes for other scenes

gameLoop :: IO ()
gameLoop = do
    let screenHeight = 1080
    let assetFiles =    [("monster", "./assets/baron.bmp")
                        ,("player", "./assets/player.bmp")
                        ,("stone_tile", "./assets/stone_tile.bmp")
                        ,("ground_top", "./assets/ground_top.bmp")
                        ,("ground", "./assets/ground.bmp")
                        ,("bullet", "./assets/bullet.bmp")
                        ]
    
    assets <- mapM (\(name, path) -> fmap (name,) (loadBMP path)) assetFiles
    let imageMap = Map.fromList assets

    play FullScreen 
        black -- background
        60 -- FPS
        initialGameState { assets = imageMap}
        (renderState screenHeight) -- Render state with screen height
        (\e s -> execState (handleGameInput e) s) -- Handle input
        updateGameState -- Updates state per frame
