module Reactivity.Input
  ( RawInput,
  parseRawInput,
  AppInput(..)
  ) where

-- inspired by the Yampa-Mandelbrot tutorial.

import FRP.Yampa ( SF, Event, accumHoldBy )
import qualified SDL
import Reactivity.MouseInput
import Interface.RawInput
import Data.Int (Int32)
type MousePos = (Int32, Int32)

-- the AppInput: i.e. the input we actually care about
data AppInput = AppInput
  { inpMouseLeft   :: !(Maybe MousePos)
  , inpMouseRight  :: !(Maybe MousePos)
  , inpQuit        :: !Bool
  , inpClear       :: !Bool
  , inpToggleWater :: !Bool
  }

initAppInput :: AppInput
initAppInput =
  AppInput
    { inpMouseLeft = Nothing
    , inpMouseRight = Nothing
    , inpQuit = False
    , inpClear = False
    , inpToggleWater = False
    }

-- how the app input changes given the win input
parseRawInput :: SF (Event RawInput) AppInput
parseRawInput = accumHoldBy nextAppInput initAppInput

nextAppInput :: AppInput -> RawInput -> AppInput
nextAppInput appInput (MouseButtonEvent ev) =
    let mousePos = mouseButtonEventPos ev
        button = mouseButtonEventButton ev
        motion = mouseButtonEventMotion ev
        in case (button, motion) of
            (ButtonLeft, Released) -> appInput {inpMouseLeft = Nothing}
            (ButtonRight, Released) -> appInput {inpMouseRight = Nothing}
            (ButtonLeft, Pressed) -> appInput {inpMouseLeft = Just mousePos}
            (ButtonRight, Pressed) -> appInput {inpMouseRight = Just mousePos}
nextAppInput appInput (MouseMotionEvent ev) =
  case (inpMouseLeft appInput, inpMouseRight appInput) of
   (Just _, Just _) -> appInput {inpMouseLeft = mousePos, inpMouseRight = mousePos}
   (Nothing, Just _) -> appInput {inpMouseRight = mousePos}
   (Just _, Nothing) -> appInput {inpMouseLeft = mousePos}
   _ -> appInput
  where mousePos = Just $ mouseMotionEventPos ev
nextAppInput appInput (KeyboardEvent ev) =
  let motion = keyboardEventKeyMotion ev
      scancode = keyboardScancode ev in
    case scancode of
        ScancodeEscape -> appInput { inpQuit = True }
        ScancodeC -> appInput { inpClear = motion == Pressed }
        ScancodeT -> appInput { inpToggleWater = motion == Pressed }
        _ -> appInput
nextAppInput appInput _ = appInput
