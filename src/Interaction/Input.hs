module Interaction.Input
  ( WinInput,
  parseWinInput,
  AppInput(..)
  ) where

-- taken mostly from the SDL/Yampa Mandelbrot tutorial.

import FRP.Yampa ( SF, Event, accumHoldBy )
import qualified SDL
import SDL.Vect (Point(P))
import           Linear (V2(..))
import Interface.UserInput

type WinInput = Event SDL.EventPayload -- SDL events

-- the AppInput: i.e. the input we actually care about
data AppInput = AppInput
  { inpMouseLeft   :: !(Maybe (Double, Double))
  , inpMouseRight  :: !(Maybe (Double, Double))
  , inpQuit        :: !Bool
  , inpKeyPressed  :: !(Maybe SDL.Scancode)
  , inpKeyReleased :: !(Maybe SDL.Scancode)
  }

initAppInput :: AppInput
initAppInput =
  AppInput
    { inpMouseLeft = Nothing
    , inpMouseRight = Nothing
    , inpQuit = False
    , inpKeyPressed = Nothing
    , inpKeyReleased = Nothing
    }

-- how the app input changes given the win input
parseWinInput :: SF WinInput AppInput
parseWinInput = accumHoldBy nextAppInput initAppInput

extractMousePos :: Integral b => Point V2 b -> (Double, Double)
extractMousePos (P (V2 x y)) = (fromIntegral x, fromIntegral y)

nextAppInput :: AppInput -> SDL.EventPayload -> AppInput
-- TODO: BUG OPPORTUNITY: does the position not change if the mouse doesn't move?
nextAppInput appInput (SDL.MouseButtonEvent ev) =
    let mousePos = extractMousePos (SDL.mouseButtonEventPos ev)
        button = SDL.mouseButtonEventButton ev
        motion = SDL.mouseButtonEventMotion ev
        in case (button, motion) of
            (SDL.ButtonLeft, SDL.Released) -> appInput {inpMouseLeft = Nothing}
            (SDL.ButtonRight, SDL.Released) -> appInput {inpMouseRight = Nothing}
            (SDL.ButtonLeft, SDL.Pressed) -> appInput {inpMouseLeft = Just mousePos}
            (SDL.ButtonRight, SDL.Pressed) -> appInput {inpMouseRight = Just mousePos}
nextAppInput appInput (SDL.MouseMotionEvent ev) =
  case inpMouseLeft appInput of
    Just _ -> appInput {inpMouseLeft = Just $ extractMousePos (SDL.mouseMotionEventPos ev)}
    Nothing -> appInput 
nextAppInput appInput (SDL.KeyboardEvent ev) =
    case scancode ev of
        SDL.ScancodeEscape -> appInput { inpQuit = True }
        _ -> appInput -- could put more if want to.
    where scancode = SDL.keysymScancode . SDL.keyboardEventKeysym
nextAppInput appInput _ = appInput
