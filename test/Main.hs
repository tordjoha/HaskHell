module Main (main) where

import Test.Hspec
import Types (GameState(..), Scene(..), MenuOption(..))
import Rendering (drawElapsedTime)

main :: IO ()
main = hspec $ do
  describe "GameState" $ do
    it "initializes with the correct default values" $ do
      let initialState = GameState
            { currentScene = MenuScene Play
            , playerScore = 0
            , elapsedTime = 0
            , playerPosition = (0, -290)
            , enemyPositions = []
            , keyStates = []
            , assets = mempty
            , projectiles = []
            , lastShotFired = False
            }
      currentScene initialState `shouldBe` MenuScene Play
      playerScore initialState `shouldBe` 0
      elapsedTime initialState `shouldBe` 0

  describe "Rendering" $ do
    it "drawElapsedTime renders the correct time text" $ do
      let picture = drawElapsedTime 32.0
      show picture `shouldContain` "32s"
