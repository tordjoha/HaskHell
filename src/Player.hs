module Player (moveLeft, moveRight) where

import Types (GameState(..))

moveLeft :: GameState -> GameState
moveLeft gameState@GameState{playerPosition = (x, y)} =
    gameState { playerPosition = (x - 3, y) }  -- Move left

moveRight :: GameState -> GameState
moveRight gameState@GameState{playerPosition = (x, y)} =
    gameState { playerPosition = (x + 3, y) }  -- Move right
