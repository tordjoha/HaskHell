module Game (gameLoop) where

import Control.Monad.State
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Map as Map

import Types (GameState(..), Scene(..), MenuOption(..), Key(..), GameMonad)
import Menu (selectMenuOption, navigateMenuUp, navigateMenuDown)
import Enemy (moveEnemyTowards)
import Colors (armyGreen, groundColor)

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

updateGame :: Event -> GameMonad ()
updateGame (EventKey (SpecialKey key) Down _ _) = do
    state <- get
    modify $ \s -> s { keyStates = key : keyStates s }
updateGame (EventKey (SpecialKey key) Up _ _) = do
    state <- get
    modify $ \s -> s { keyStates = filter (/= key) (keyStates s) }
updateGame _ = return ()

moveLeft :: GameState -> GameState
moveLeft gameState@GameState{playerPosition = (x, y)} =
    gameState { playerPosition = (x - 3, y) }  -- Move left

moveRight :: GameState -> GameState
moveRight gameState@GameState{playerPosition = (x, y)} =
    gameState { playerPosition = (x + 3, y) }  -- Move right

drawElapsedTime :: Float -> Picture
drawElapsedTime time = translate (-80) 300 $ scale 0.2 0.2 $ color white $ text ("Time: " ++ show (floor time) ++ "s")

renderState :: Float -> GameState -> Picture
renderState screenHeight GameState{currentScene = PlayScene, playerPosition = (x, y), enemyPositions = enemies, elapsedTime = elapsed, assets = assets} = pictures
    [ color groundColor $ translate 0 (-screenHeight / 2 + 50) $ rectangleSolid 5000 300  -- Platform
    , translate x y $ scale 0.2 0.2 (maybe Blank id (Map.lookup "player" assets)) -- Draw player as player.bmp
    , pictures (map (\(ex, ey) -> translate ex ey $ scale 0.2 0.2 (maybe Blank id (Map.lookup "monster" assets))) enemies) -- Draw enemies as monsters
    , drawElapsedTime elapsed
    ]
renderState _ GameState{currentScene = MenuScene item} = pictures
    [ translate 0 150 $ color white $ scale 0.5 0.5 $ text "HaskHell"
    , translate 0 50 $ color (menuColor item Play) $ scale 0.3 0.3 $ text "Play"
    , translate 0 (-50) $ color (menuColor item Exit) $ scale 0.3 0.3 $ text "Exit"
    ]
  where
    menuColor selected current = if selected == current then red else white 

update :: Float -> GameState -> GameState
update dt state@GameState{currentScene = PlayScene, enemyPositions = enemies, playerPosition = playerPos@(px, py)} =
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
update _ state = state  -- No changes for other scenes

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
        (\e s -> execState (updateGame e) s) -- Handle input
        update -- Updates state per frame
