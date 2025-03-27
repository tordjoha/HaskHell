module Game
    ( gameLoop
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Scene
    = Intro
    | Game
    | GameOver

data World = World
    { scene :: Scene
    , score :: Int
    , time :: Float
    , menuSelection :: MenuItem
    }

data MenuItem = Play | Exit
    deriving (Eq, Show)

renderIntro :: World -> Picture
renderIntro world = pictures
    [ translate (-150) 150 $ color white $ scale 0.8 0.8 $ text "HaskHell"
    , translate (-80) 20  $ color (menuColor Play) $ scale 0.3 0.3 $ text "Play Game"
    , translate (-80) (-50) $ color (menuColor Exit) $ scale 0.3 0.3 $ text "Exit"
    ]
  where
    menuColor item = if item == menuSelection world then red else white

renderGame :: World -> Picture
renderGame _ = color green (rectangleSolid 100 100)

renderGameOver :: World -> Picture
renderGameOver _ = color blue (rectangleSolid 100 100)


render :: World -> Picture
render world = case scene world of 
    Intro -> renderIntro world
    Game -> renderGame world
    GameOver -> renderGameOver world

handleInput :: Event -> World -> World
handleInput (EventKey (SpecialKey KeyEsc) Down _ _) _ = error "Exit"
handleInput (EventKey (SpecialKey KeyUp) Down _ _) world@World{scene=Intro} = 
    world { menuSelection = Play }
handleInput (EventKey (SpecialKey KeyDown) Down _ _) world@World{scene=Intro} = 
    world { menuSelection = Exit }
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) world@World{scene=Intro} =
    case menuSelection world of
        Play -> world { scene = Game }
        Exit -> error "Exit"
handleInput _ world = world

update :: Float -> World -> World
update _ = id

fps :: Int
fps = 60

initialWorld :: World
initialWorld = World 
    { scene = Intro
    , score = 0
    , time = 0
    , menuSelection = Play
    }
gameLoop :: IO ()
gameLoop = play window background fps initialWorld render handleInput update
    where
        window = FullScreen
        background = black
