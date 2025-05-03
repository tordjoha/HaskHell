module Rendering (drawElapsedTime, renderState) where
import Graphics.Gloss (Picture(..), color, translate, scale, text, rectangleSolid, pictures, white, red)
import Types (GameState(..), Scene(..), MenuOption(..))
import Colors (groundColor)
import qualified Data.Map as Map

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
