module Main where

import Simulation.State
import Utils.Matrix

import Test.Tasty
import Test.Tasty.Hspec as Hspec
import Test.Hspec


main = do
  spec <- Hspec.testSpec "spec" spec_nextState
  defaultMain
    (testGroup "tests"
      [ spec
      ])

spec_nextState :: Spec
spec_nextState = 
    describe "nextState" $ do
        it "should have a empty next state if current state is empty" $
            let emptyState = initialState 5
                dims = dimsFromN 5
                zeroMat = Just $ matrixInit dims 0 in
            nextState zeroMat emptyState `shouldBe` emptyState