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
import Colors (armyGreen, groundColor, blackTranslucent)
import Rendering (drawElapsedTime, renderState)

initialGameState :: Map.Map String Picture -> GameState
initialGameState loadedAssets = GameState
    { currentScene = MenuScene Play
    , playerScore = 0
    , elapsedTime = 0
    , playerPosition = (0, -290)
    , enemyPositions = []
    , keyStates = []
    , assets = loadedAssets
    , projectiles = []
    }

handleGameInput :: Event -> GameMonad ()
handleGameInput (EventKey (SpecialKey KeyEnter) Down _ _) = do
    state <- get
    case currentScene state of
        GameOverScene -> put (initialGameState (assets state)) { currentScene = PlayScene } -- Restart directly into PlayScene
        MenuScene Play -> modify selectMenuOption
        MenuScene Exit -> modify selectMenuOption
        _ -> return ()
handleGameInput (EventKey (SpecialKey KeyUp) Down _ _) = do
    state <- get
    when (currentScene state == MenuScene Play || currentScene state == MenuScene Exit) $
        modify navigateMenuUp
handleGameInput (EventKey (SpecialKey KeyDown) Down _ _) = do
    state <- get
    when (currentScene state == MenuScene Play || currentScene state == MenuScene Exit) $
        modify navigateMenuDown
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
        canShoot = KeyTab `elem` keyStates state && not (lastShotFired state)
        updatedState = if canShoot
                      then shootProjectile playerMovementState { lastShotFired = True }
                      else playerMovementState { lastShotFired = KeyTab `elem` keyStates state }
        movedProjectiles = [(x + 10, y) | (x, y) <- projectiles updatedState]
        spawnInterval = 5
        newElapsed = elapsedTime updatedState
        spawnPosition = (900, -310)
        newEnemies = if floor newElapsed `mod` spawnInterval == 0
                     then enemies ++ [spawnPosition]
                     else enemies
        movedEnemies = map (moveEnemyTowards playerPos) newEnemies
        (remainingEnemies, remainingProjectiles) = resolveCollisions movedEnemies movedProjectiles
        collisionDetected = any (\(ex, ey) -> abs (px - ex) < 30 && abs (py - ey) < 30) remainingEnemies
     in if collisionDetected
       then state { currentScene = GameOverScene }
       else updatedState { enemyPositions = remainingEnemies, projectiles = remainingProjectiles }
  where
    applyMovementKeys :: SpecialKey -> GameState -> GameState
    applyMovementKeys KeyLeft = moveLeft
    applyMovementKeys KeyRight = moveRight
    applyMovementKeys _ = id

    resolveCollisions :: [(Float, Float)] -> [(Float, Float)] -> ([(Float, Float)], [(Float, Float)])
    resolveCollisions enemies bullets =
        let isHit (ex, ey) = any (\(bx, by) -> abs (bx - ex) < 20 && abs (by - ey) < 20) bullets
            remainingEnemies = filter (not . isHit) enemies
            remainingBullets = filter (\(bx, by) -> not (any (\(ex, ey) -> abs (bx - ex) < 20 && abs (by - ey) < 20) enemies)) bullets
        in (remainingEnemies, remainingBullets)
updateGameState _ state = state  -- No changes for other scenes

loadAssets :: IO (Map.Map String Picture)
loadAssets = do
    let assetFiles = [("monster", "./assets/baron.bmp")
                     ,("player", "./assets/player.bmp")
                     ,("stone_tile", "./assets/stone_tile.bmp")
                     ,("ground_top", "./assets/ground_top.bmp")
                     ,("ground", "./assets/ground.bmp")
                     ,("bullet", "./assets/bullet.bmp")
                     ]
    assets <- mapM (\(name, path) -> fmap (name,) (loadBMP path)) assetFiles
    return $ Map.fromList assets

gameLoop :: IO ()
gameLoop = do
    let screenHeight = 1080
    loadedAssets <- loadAssets -- Load assets
    play FullScreen 
        black -- background color
        60 -- FPS
        (initialGameState loadedAssets) -- Pass assets to initialGameState
        (renderState screenHeight) -- Render the game state
        (\e s -> execState (handleGameInput e) s) -- Handle input
        updateGameState -- Update the game state per frame
