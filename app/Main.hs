module Main where

import qualified Game (gameLoop)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Game.gameLoop
