module Rendering (drawElapsedTime, renderState) where
import Graphics.Gloss (Picture(..), color, translate, scale, text, rectangleSolid, pictures, white, red)
import Types (GameState(..), Scene(..), MenuOption(..))
import Colors (armyGreen, groundColor, blackTranslucent)
import qualified Data.Map as Map

tileBackground :: Maybe Picture -> Picture
tileBackground Nothing = Blank
tileBackground (Just pic) = pictures [translate (fromIntegral x * tileSize) (fromIntegral y * tileSize) scaledPic | x <- [-cols..cols], y <- [-rows..rows]]
  where
    tileSize = 75
    scaledPic = scale 0.2 0.2 pic
    screenWidth = 3840
    screenHeight = 2160
    cols = floor (screenWidth / (tileSize * 2))
    rows = floor (screenHeight / (tileSize * 2))

repeatTextureHorizontal :: Maybe Picture -> Picture
repeatTextureHorizontal Nothing = Blank
repeatTextureHorizontal (Just pic) = pictures [translate (fromIntegral x * adjustedTileSize) 0 scaledPic | x <- [-cols..cols]]
  where
    tileSize = 75
    overlap = 13
    adjustedTileSize = tileSize - overlap
    scaledPic = scale 0.2 0.2 pic
    screenWidth = 3840
    cols = floor (screenWidth / (adjustedTileSize * 2))
  
drawElapsedTime :: Float -> Picture
drawElapsedTime time = pictures 
  [ translate (-80) 300 $ scale 0.3 0.3 $ color white $ text "Time: "
  , translate 30 300 $ scale 0.3 0.3 $ color red $ text (show (floor time) ++ "s")
  ]

renderState :: Float -> GameState -> Picture
renderState _ GameState{currentScene = GameOverScene} = pictures
    [ color blackTranslucent $ rectangleSolid 800 600
    , translate (-280) 50 $ color red $ scale 1 1 $ pictures [translate dx dy $ text "Game Over" | dx <- [-2, -1, 0, 1, 2], dy <- [-2, -1, 0, 1, 2]]
    , translate (-250) (-50) $ color white $ scale 0.3 0.3 $ text "Press Enter to Play Again"
    , translate (-250) (-150) $ color white $ scale 0.3 0.3 $ text "Press Esc to Exit"
    ]
renderState screenHeight GameState{currentScene = PlayScene, playerPosition = (x, y), enemyPositions = enemies, elapsedTime = elapsed, assets = assets, projectiles = ps} = pictures
    [ maybe Blank id (fmap (translate 0 (-screenHeight / 2 + 168) . repeatTextureHorizontal . Just) (Map.lookup "ground_top" assets)) -- Platformtop
    , maybe Blank id (fmap (translate 0 (-screenHeight / 2 + 110) . repeatTextureHorizontal . Just) (Map.lookup "ground" assets)) -- Platform rest
    , maybe Blank id (fmap (translate 0 (-screenHeight / 2 + 52) . repeatTextureHorizontal . Just) (Map.lookup "ground" assets)) -- Platform rest
    , translate x y $ scale 0.3 0.3 (maybe Blank id (Map.lookup "player" assets))
    , pictures (map (\(ex, ey) -> translate ex (ey + 30) $ scale 0.2 0.2 (maybe Blank id (Map.lookup "monster" assets))) enemies) -- Draw enemies as monsters
    , pictures [translate (px + 15) (py + 16) $ scale 0.1 0.1 (maybe Blank id (Map.lookup "bullet" assets)) | (px, py) <- ps] -- Draw projectiles as bullets
    , translate (-600) (screenHeight / 2 - 260) $ scale 0.2 0.2 $ color white $ text "Move: Left Arrow / Right Arrow"
    , translate (-600) (screenHeight / 2 - 300) $ scale 0.2 0.2 $ color white $ text "Shoot: Press Tab"
    , drawElapsedTime elapsed
    ]
renderState _ GameState{currentScene = MenuScene item, assets = assets} = pictures
    [ tileBackground (Map.lookup "stone_tile" assets) -- Menu background
    , translate (-150) 0 $ color white $ scale 0.7 0.7 $ pictures [translate dx dy $ text "HaskHell" | dx <- [-2, -1, 0, 1, 2], dy <- [-2, -1, 0, 1, 2]]
    , translate (-150) (-100) $ color (menuColor item Play) $ scale 0.3 0.3 $ pictures [translate dx dy $ text "Play" | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]
    , translate (-150) (-200) $ color (menuColor item Exit) $ scale 0.3 0.3 $ pictures [translate dx dy $ text "Exit" | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]
    ]
  where
    menuColor selected current = if selected == current then red else white
renderState _ _ = Blank
