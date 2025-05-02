module Main where

import qualified Game (gameLoop)

main :: IO ()
main = do
  putStrLn "Starting game..."
  Game.gameLoop
