module Player (moveLeft, moveRight, shootProjectile) where

import Types (GameState(..))

moveLeft :: GameState -> GameState
moveLeft gameState@GameState{playerPosition = (x, y)} =
    gameState { playerPosition = (x - 3, y) }  -- Move left

moveRight :: GameState -> GameState
moveRight gameState@GameState{playerPosition = (x, y)} =
    gameState { playerPosition = (x + 3, y) }  -- Move right

shootProjectile :: GameState -> GameState
shootProjectile gameState@GameState{playerPosition = (x, y), projectiles = ps} =
    gameState { projectiles = ps ++ [(x, y)] }  -- Add a new projectile at the player's position
    