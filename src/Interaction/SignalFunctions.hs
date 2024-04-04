{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}

module Interaction.SignalFunctions
  ( signalFunction
  , RenderOutput(..)
  ) where

import           Data.Functor
import           FRP.Yampa
import           GHC.Int             (Int32)
import           Interaction.Input
import           Interface.UserInput
import qualified SDL
import           Simulation.Source   (Source, getSourceFromMouseInput)
import           Simulation.State
import Data.Tuple (swap)

type WinSize = (Int, Int)

data KeyboardInfo
  = Quit
  | ClearCanvas
  | ToggleWater

data RenderOutput
  = QuitCanvas
  | DefaultCanvas State
  | CanvasWaterVisible State

data DiffusionTacticModifier
  = MousePress MouseButtonPress
  | EraseToggle

data MouseButtonPress
  = LeftPress | RightPress
  deriving (Eq)

-------- UTILITY SIGNAL FUNCTIONS --------
edgy :: SF a (Event b) -> SF a (Event b)
edgy sf = (sf >>> arr isEvent >>> edge) &&& sf >>> arr (uncurry (>>))

-------- INPUT HANDLING SIGNAL FUNCTIONS --------
appInputSF :: SF (RawInput, WinSize) (AppInput, WinSize)
appInputSF = first parseRawInput

-- takes canvas size and initial mouse input, and produces a SF (AppInput, WinSize) MouseInput
mouseInputSF :: Int -> MouseInput -> SF (AppInput, WinSize) MouseInput
mouseInputSF canvasSize = sscan (toMouseInput canvasSize)
  where
    toMouseInput canvasSize prevMouse (appInput, screenSize) =
      let getNextMouseInput x y =
            getMouseInput
              True
              ((floor x, floor y), canvasSize, screenSize)
              prevMouse
       in case (inpMouseLeft appInput, inpMouseRight appInput) of
            (Just (x, y), _) -> getNextMouseInput x y
            (Nothing, Just (x, y)) -> (getNextMouseInput x y) {mouseDown = False, mouseRightDown = True}
            _ -> prevMouse {mouseDown = False, mouseRightDown = False}

mouseToSourceSF ::  SF (MouseInput, (Int, Int)) (Maybe Source)
mouseToSourceSF = arr (uncurry getSourceFromMouseInput)

mouseToEventSF :: SF MouseInput (Event MouseButtonPress)
mouseToEventSF = proc mouseInput -> do
  leftEvent <- edgeTag LeftPress -< mouseDown mouseInput
  rightEvent <- edgeTag RightPress -< mouseRightDown mouseInput
  returnA -< case leftEvent of
                NoEvent -> rightEvent
                _ -> leftEvent

diffuseSF :: Bool -> SF (State, MouseInput) State
diffuseSF leftPressed =
  difuseHelperSF `switch` (\case
                              LeftPress -> diffuseSF True
                              RightPress -> diffuseSF False)
    where mapToSrc = proc (state, mouseIn) -> do
            mSrc <- mouseToSourceSF -< (mouseIn, canvasDims state)
            returnA -< (mSrc, state)
          diffusionTacticSF = mapToSrc >>> arr (uncurry $ nextState leftPressed)
          difuseHelperSF = diffusionTacticSF &&& (second (edgy mouseToEventSF) >>> arr snd)

-- diffuseSF :: Bool -> SF (State, MouseInput) State
-- diffuseSF leftPressed =
--   difuseHelperSF `switch` (\case
--                               LeftPress -> diffuseSF True
--                               RightPress -> diffuseSF False)
--     where mapToSrc = proc (state, mouseIn) -> do
--             mSrc <- mouseToSourceSF -< (mouseIn, canvasDims state)
--             returnA -< (mSrc, state)
--           diffusionTacticSF = mapToSrc >>> arr (uncurry $ nextState leftPressed)
--           difuseHelperSF = diffusionTacticSF &&& (second (edgy mouseToEventSF) >>> arr snd)

-- chooseDiffusionTacticSF :: DiffusionTacticModifier -> SF (State, MouseInput) State
-- chooseDiffusionTacticSF modifier =
--   case modifier of
--     MousePress mouseButton -> mapToSrc >>> arr (uncurry $ nextState (mouseButton == LeftPress))
--   where mapToSrc = proc (state, mouseIn) -> do
--             mSrc <- mouseToSourceSF -< (mouseIn, canvasDims state)
--             returnA -< (mSrc, state)

-------- RENDER SIGNAL FUNCTIONS --------
canvasStateSF :: RenderOutput -> SF (State, Event a) RenderOutput
canvasStateSF =
  sscan
    (\prevRenderTactic (newState, waterToggleEvent) ->
       case waterToggleEvent of
         NoEvent -> updateRenderState prevRenderTactic newState
         _       -> toggleWaterVisibility prevRenderTactic newState)
  where
    toggleWaterVisibility renderTactic state =
      case renderTactic of
        DefaultCanvas state      -> CanvasWaterVisible state
        CanvasWaterVisible state -> DefaultCanvas state
        _                        -> renderTactic
    updateRenderState renderTactic state =
      case renderTactic of
        DefaultCanvas _      -> DefaultCanvas state
        CanvasWaterVisible _ -> CanvasWaterVisible state
        _                    -> renderTactic

-- given an initial state, generates a signal function that takes any state and a toggle event and gives you a render output
renderOutputFromState :: State -> SF (State, Event a) RenderOutput
renderOutputFromState = canvasStateSF . initialRenderOutput
  where
    initialRenderOutput = DefaultCanvas

-------- TYING THE ABOVE TOGETHER INTO A BIG SIGNAL FUNCTION

-- the overall signal function.
signalFunction :: State -> MouseInput -> SF (RawInput, WinSize) RenderOutput
signalFunction initialState initialMouseIn =
  first parseRawInput >>> keyboardHandledRenderOutputSF initialState initialMouseIn

-- Given an initial state and an initial mouse in, generates a state from an AppInput and WinSize.
diffuseWithMouseInput :: State -> MouseInput -> SF (AppInput, WinSize) State
diffuseWithMouseInput initialState initialMouseIn =
  mouseInputSF (canvasSize initialState) initialMouseIn >>> loopPre initialState (swapSF (diffuseSF True) >>> arr dup)
    where swapSF = (arr swap >>>)

-- handles keyboard events that drastically change program output.
keyboardHandledRenderOutputSF :: State -> MouseInput -> SF (AppInput, WinSize) RenderOutput
keyboardHandledRenderOutputSF state inputs =
  diffuseSF state inputs
    `switch` (\case
                Quit -> arr (const QuitCanvas)
                ClearCanvas -> keyboardHandledRenderOutputSF state inputs)
  where
    diffuseSF initialState initialMouse =
      diffusedRenderOutputSF initialState initialMouse &&& edgy (arr (appInToKeyboardInfo . fst))

-- gives the rendering output of the diffusion
diffusedRenderOutputSF :: State -> MouseInput -> SF (AppInput, WinSize) RenderOutput
diffusedRenderOutputSF initialState initialMouse = proc appInfo@(appInput, winSize) -> do
  waterToggleEvent <- appInToToggleInfo -< appInput
  state <- diffuseWithMouseInput initialState initialMouse -< appInfo
  renderOut <- renderOutputFromState initialState -< (state, waterToggleEvent)
  returnA -< renderOut
  where appInToToggleInfo = arr inpToggleWater >>> edge

appInToKeyboardInfo :: AppInput -> Event KeyboardInfo
appInToKeyboardInfo appInput
  | inpQuit appInput = Event Quit
  | inpClear appInput = Event ClearCanvas
  | otherwise = NoEvent