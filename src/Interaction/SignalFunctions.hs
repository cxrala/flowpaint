{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}

module Interaction.SignalFunctions
  ( signalFunction
  , RenderOutput(..)
  ) where

import           Data.Functor
import           Debug.Trace         (trace)
import           FRP.Yampa
import           GHC.Int             (Int32)
import           Interaction.Input
import           Interface.UserInput
import qualified SDL
import           Simulation.Source   (Source, getSourceFromMouseInput)
import           Simulation.State

type WinSize = (Int, Int)

data KeyboardInfo
  = Quit
  | ClearCanvas
  | ToggleWater

data RenderOutput
  = QuitCanvas
  | DefaultCanvas State
  | CanvasWaterVisible State

data MouseButtonPress
  = LeftPress | RightPress

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

diffusionTacticSF :: SF (State, MouseInput) State
diffusionTacticSF =
  second mouseToEventSF `switch` diffusionTactic
    where diffusionTactic LeftPress = mapToSrc >>> arr (uncurry $ nextState True)
          diffusionTactic RightPress = mapToSrc >>> arr (uncurry $ nextState False)
          mapToSrc = proc (state, mouseIn) -> do
            mSrc <- mouseToSourceSF -< (mouseIn, canvasDims state)
            returnA -< (mSrc, state)

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

signalFunction :: State -> MouseInput -> SF (RawInput, WinSize) RenderOutput
signalFunction initialState initialMouseIn =
  first parseRawInput >>> foo initialState initialMouseIn

diffuseWithMouseInput :: State -> MouseInput -> SF (AppInput, WinSize) State
diffuseWithMouseInput initialState initialMouseIn =
  sscan (toMouseInput $ canvasSize initialState) initialMouseIn
    >>> sscan inputToNextState initialState

edgy :: SF a (Event b) -> SF a (Event b)
edgy sf = (sf >>> arr isEvent >>> edge) &&& sf >>> arr (uncurry (>>))

-- runs "the default diffusion signal" until the keyboard event occurs, then runs diffuseWithMouseInput
foo :: State -> MouseInput -> SF (AppInput, WinSize) RenderOutput
foo state inputs =
  diffuseSF state inputs
    `switch` (\case
                Quit -> arr (const QuitCanvas)
                -- ToggleWater prevState -> diffuseSF prevSta input
                ClearCanvas -> foo state inputs)
  where
    diffuseSF initialState initialMouse =
      (diffuseWithMouseInput initialState initialMouse
         >>> arr (\x -> CanvasState (x, True)))
        &&& edgy (arr (appInToKeyboardInfo . fst))

-- SF (RawInput, WinSize) State -> SF (AppInput, WinSize) (Maybe State)
appInToKeyboardInfo :: AppInput -> Event KeyboardInfo
appInToKeyboardInfo appInput
  | inpQuit appInput = Event Quit
  | inpClear appInput = Event ClearCanvas
  -- | inpToggleWater appInput = Event ToggleWater
  | otherwise = NoEvent

inputToNextState :: State -> MouseInput -> State
inputToNextState prevState mouseIn =
  nextState
    (mouseDown mouseIn)
    (getSourceFromMouseInput mouseIn (canvasDims prevState))
    prevState
