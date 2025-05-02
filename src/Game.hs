module Game (gameLoop) where

import Control.Monad.State
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Map as Map

data Key = Special SpecialKey | Char Char
    deriving (Show, Eq)

data GameState = GameState
    { currentScene :: Scene
    , playerScore :: Int
    , elapsedTime :: Float
    , playerPosition :: (Float, Float)
    , enemyPositions :: [(Float, Float)] -- List of enemies positions
    , keyStates :: [SpecialKey]
    , assets :: Map.Map String Picture -- Stores all image assets
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

initialGameState :: GameState
initialGameState = GameState
    { --currentScene = MenuScene Play
    -- ADD THIS BACK FOR SHOWING MENU
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

-- MENU NAVIGATION
navigateMenuUp :: GameState -> GameState
navigateMenuUp gs@GameState{currentScene = MenuScene _} = 
    gs { currentScene = MenuScene Play }
navigateMenuUp gs = gs

navigateMenuDown :: GameState -> GameState
navigateMenuDown gs@GameState{currentScene = MenuScene _} = 
    gs { currentScene = MenuScene Exit }
navigateMenuDown gs = gs

selectMenuOption :: GameState -> GameState
selectMenuOption gs@GameState{currentScene = MenuScene Play} = 
    gs { currentScene = PlayScene }
selectMenuOption gs@GameState{currentScene = MenuScene Exit} = 
    error "Exit game"
selectMenuOption gs = gs

-- PLAYER NAVIGATION
--moveUp :: GameState -> GameState
--moveUp gameState@GameState{playerPosition = (x, y)} =
--    gameState { playerPosition = (x, y + 5) }  -- Move up

--moveDown :: GameState -> GameState
--moveDown gameState@GameState{playerPosition = (x, y)} =
--    gameState { playerPosition = (x, y - 5) }  -- Move down

moveLeft :: GameState -> GameState
moveLeft gameState@GameState{playerPosition = (x, y)} =
    gameState { playerPosition = (x - 3, y) }  -- Move left

moveRight :: GameState -> GameState
moveRight gameState@GameState{playerPosition = (x, y)} =
    gameState { playerPosition = (x + 3, y) }  -- Move right

-- Colors
armyGreen :: Color
armyGreen = makeColorI 78 91 49 255

groundColor :: Color
groundColor = makeColorI 107 68 35 255

drawPlayer :: (Float, Float) -> Picture
drawPlayer (x, y) = pictures
    [ translate x y $ color armyGreen (rectangleSolid 25 25)  -- Body
    , translate (x) (y + 25) $ color armyGreen (circleSolid 14)  -- Helmet
    , translate (x) (y + 25) $ color white (circleSolid 9)  -- Head
    , translate (x - 15) (y+2) $ color black (rectangleSolid 7 20)  -- Left arm
    , translate (x + 15) (y+2) $ color black (rectangleSolid 7 20)  -- Right arm
    , translate (x - 8) (y - 20) $ color armyGreen (rectangleSolid 8 20)  -- Left leg
    , translate (x + 8) (y - 20) $ color armyGreen (rectangleSolid 8 20)  -- Right leg
    ]

drawEnemy :: (Float, Float) -> Picture
drawEnemy (x, y) = pictures
    [ translate x y $ color armyGreen (rectangleSolid 25 25)  -- Body
    , translate (x) (y + 25) $ color red (circleSolid 14)  -- Helmet
    , translate (x) (y + 25) $ color red (circleSolid 9)  -- Head
    , translate (x - 15) (y+2) $ color red (rectangleSolid 7 20)  -- Left arm
    , translate (x + 15) (y+2) $ color red (rectangleSolid 7 20)  -- Right arm
    , translate (x - 8) (y - 20) $ color red (rectangleSolid 8 20)  -- Left leg
    , translate (x + 8) (y - 20) $ color red (rectangleSolid 8 20)  -- Right leg
    ]

moveEnemyTowards :: (Float, Float) -> (Float, Float) -> (Float, Float)
moveEnemyTowards (px, py) (ex, ey) =
    let dx = px - ex
        dy = py - ey
        distance = sqrt (dx^2 + dy^2)
        speed = 0.5  -- Enemy speed
        stepX = if distance > 0 then dx / distance * speed else 0
        stepY = if distance > 0 then dy / distance * speed else 0
    in (ex + stepX, ey + stepY)

drawElapsedTime :: Float -> Picture
drawElapsedTime time = translate (-80) 300 $ scale 0.2 0.2 $ color white $ text ("Time: " ++ show (floor time) ++ "s")

renderState :: Float -> GameState -> Picture
renderState screenHeight GameState{currentScene = PlayScene, playerPosition = (x, y), enemyPositions = enemies, elapsedTime = elapsed, assets = assets} = pictures
    [ color groundColor $ translate 0 (-screenHeight / 2 + 50) $ rectangleSolid 5000 300  -- Platform
    , drawPlayer(x, y)
    , translate 0 0 $ scale 0.3 0.3 (maybe Blank id (Map.lookup "monster" assets))
    --, drawEnemy(0, 0)
    , pictures (map drawEnemy enemies) -- Draw enemies
    , drawElapsedTime elapsed
    ]
renderState _ GameState{currentScene = MenuScene item} = pictures
    [ translate 0 150 $ color white $ scale 0.5 0.5 $ text "HaskHell"
    , translate 0 50 $ color (menuColor item Play) $ scale 0.3 0.3 $ text "Play"
    , translate 0 (-50) $ color (menuColor item Exit) $ scale 0.3 0.3 $ text "Exit"
    ]
  where
    menuColor selected current = if selected == current then red else white --renderState GameState{currentScene = PlayScene} = 
--    color rose (rectangleSolid 25 25)
--renderState GameState{currentScene = PauseScene} = 
--    color yellow (text "Paused")
--renderState GameState{currentScene = GameOverScene} = 
--    color red (circle 100)
--renderState _ = blank

update :: Float -> GameState -> GameState
update dt state@GameState{currentScene = PlayScene, enemyPositions = enemies, playerPosition = playerPos@(px, py)} =
    let updatedState = foldr applyMovement state { elapsedTime = elapsedTime state + dt } (keyStates state)
        spawnInterval = 5  -- Spawn a new enemy every 5 seconds
        newElapsed = elapsedTime updatedState
        --spawnPosition = (px + 300, py)  -- Spawn enemy 300 units to the right of the player
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
    monster <- loadBMP "./assets/baron.bmp"
    let imageMap = Map.fromList
            [ ("monster", monster)
            ]

    play FullScreen 
        --(makeColorI 59 10 10 255) -- background
        black -- background
        60 -- FPS
        initialGameState { assets = imageMap}
        (renderState screenHeight) -- Render state with screen height
        (\e s -> execState (updateGame e) s) -- Handle input
        update -- Updates state per frame
