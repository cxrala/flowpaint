{-# LANGUAGE Arrows            #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Interaction.SignalFunctions
  ( signalFunction
  ) where

import           Data.Functor
import           Debug.Trace         (trace)
import           FRP.Yampa
import           GHC.Int             (Int32)
import           Interaction.Input
import           Interface.UserInput
import qualified SDL
import           Simulation.Source   (getSourceFromMouseInput)
import           Simulation.State

type WinSize = (Int, Int)

data KeyboardInfo
  = Quit
  | ClearCanvas

signalFunction :: State -> MouseInput -> SF (WinInput, WinSize) (Maybe State)
signalFunction initialState initialMouseIn =
  first parseWinInput >>> foo initialState initialMouseIn

diffuseWithMouseInput :: State -> MouseInput -> SF (AppInput, WinSize) State
diffuseWithMouseInput initialState initialMouseIn =
  sscan (toMouseInput $ canvasSize initialState) initialMouseIn
    >>> sscan inputToNextState initialState

edgy :: SF a (Event b) -> SF a (Event b)
edgy sf = (sf >>> arr isEvent >>> edge) &&& sf >>> arr (uncurry (>>))

-- runs "the default diffusion signal" until the keyboard event occurs, then runs diffuseWithMouseInput
foo :: State -> MouseInput -> SF (AppInput, WinSize) (Maybe State)
foo state inputs =
  diffuseSF state inputs
    `switch` (\case
                Quit -> arr (const Nothing)
                ClearCanvas -> foo state inputs)
  where
    diffuseSF initialState initialMouse =
      (diffuseWithMouseInput initialState initialMouse >>> arr Just)
        &&& edgy (arr (appInToKeyboardInfo . fst))

-- SF (WinInput, WinSize) State -> SF (AppInput, WinSize) (Maybe State)
appInToKeyboardInfo :: AppInput -> Event KeyboardInfo
appInToKeyboardInfo appInput
  | inpQuit appInput = Event Quit
  | inpClear appInput = Event ClearCanvas
  | otherwise = NoEvent

inputToNextState :: State -> MouseInput -> State
inputToNextState prevState mouseIn =
  nextState (getSourceFromMouseInput mouseIn (canvasDims prevState)) prevState

toMouseInput :: Int -> MouseInput -> (AppInput, WinSize) -> MouseInput
toMouseInput canvasSize prevMouse (appInput, screenSize) =
  case inpMouseLeft appInput of
    Nothing -> prevMouse {mouseDown = False}
    Just (x, y) ->
      getMouseInput True ((floor x, floor y), canvasSize, screenSize) prevMouse
