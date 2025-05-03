module Menu (selectMenuOption, navigateMenuUp, navigateMenuDown) where

import Types (GameState(..), Scene(..), MenuOption(..))

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
