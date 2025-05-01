module Game (gameLoop) where

import Control.Monad.State
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

data Key = Special SpecialKey | Char Char
    deriving (Show, Eq)

data GameState = GameState
    { currentScene :: Scene
    , playerScore :: Int
    , elapsedTime :: Float
    , playerPosition :: (Float, Float)
    , keyStates :: [SpecialKey]
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
    , keyStates = []
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
--    gameState { playerPosition = (x, y + 5) }  -- Move up by 10 units

--moveDown :: GameState -> GameState
--moveDown gameState@GameState{playerPosition = (x, y)} =
--    gameState { playerPosition = (x, y - 5) }  -- Move down by 10 units

moveLeft :: GameState -> GameState
moveLeft gameState@GameState{playerPosition = (x, y)} =
    gameState { playerPosition = (x - 5, y) }  -- Move left by 10 units

moveRight :: GameState -> GameState
moveRight gameState@GameState{playerPosition = (x, y)} =
    gameState { playerPosition = (x + 5, y) }  -- Move right by 10 units

armyGreen :: Color
armyGreen = makeColorI 78 91 49 255

drawPlayer :: (Float, Float) -> Picture
drawPlayer (x, y) = pictures
    [ translate x y $ color armyGreen (rectangleSolid 25 25)  -- Main rectangle (body)
    , translate (x) (y + 25) $ color white (circleSolid 13)  -- Head
    , translate (x - 15) (y+2) $ color black (rectangleSolid 7 20)  -- Left arm
    , translate (x + 15) (y+2) $ color black (rectangleSolid 7 20)  -- Right arm
    , translate (x - 8) (y - 20) $ color armyGreen (rectangleSolid 8 20)  -- Left leg
    , translate (x + 8) (y - 20) $ color armyGreen (rectangleSolid 8 20)  -- Right leg
    ]

renderState :: Float -> GameState -> Picture
renderState screenHeight GameState{currentScene = PlayScene, playerPosition = (x, y)} = pictures
    [ color (makeColorI 128 79 0 255) $ translate 0 (-screenHeight / 2 + 50) $ rectangleSolid 5000 300  -- Platform
    , drawPlayer(x, y)-- translate x y $ color armyGreen (rectangleSolid 50 50)
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
update _ state = foldr applyMovement state (keyStates state)
  where
    applyMovement :: SpecialKey -> GameState -> GameState
    -- applyMovement KeyUp = moveUp
    --applyMovement KeyDown = moveDown
    applyMovement KeyLeft = moveLeft
    applyMovement KeyRight = moveRight
    applyMovement _ = id

gameLoop :: IO ()
gameLoop = do
    let screenHeight = 1080  -- Replace with the actual screen height
    play FullScreen 
        (makeColorI 135 206 235 255) -- background
        60 -- FPS
        initialGameState
        (renderState screenHeight) -- Render state with screen height
        (\e s -> execState (updateGame e) s) -- Handle input
        update -- Updates state per frame