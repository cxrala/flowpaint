{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Interaction.SignalFunctions
  ( signalFunction
  ) where

import           FRP.Yampa
import           GHC.Int   (Int32)
import qualified SDL
import Interaction.Input
import Data.Functor
import Simulation.State
import Interface.UserInput
import Simulation.Source (getSourceFromMouseInput)
import Debug.Trace (trace)

type WinSize = (Int, Int)

signalFunction :: State -> MouseInput -> SF (WinInput, WinSize) State
signalFunction state mouseIn =
  first parseWinInput >>> sscan (toMouseInput $ canvasSize state) mouseIn >>> sscan inputToNextState state

inputToNextState :: State -> MouseInput -> State
inputToNextState prevState mouseIn =
  nextState (getSourceFromMouseInput mouseIn (canvasDims prevState)) prevState

toMouseInput :: Int -> MouseInput -> (AppInput, WinSize) -> MouseInput
toMouseInput canvasSize prevMouse (appInput, screenSize) =
  case inpMouseLeft appInput of
    Nothing -> prevMouse {mouseDown = False}
    Just (x, y) -> getMouseInput True ((floor x, floor y), canvasSize, screenSize) prevMouse