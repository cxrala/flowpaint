module Interaction.Input
  ( RawInput,
  parseRawInput,
  AppInput(..)
  ) where

-- inspired by the Yampa-Mandelbrot tutorial.

import FRP.Yampa ( SF, Event, accumHoldBy )
import qualified SDL
import SDL.Vect (Point(P))
import           Linear (V2(..))
import Interface.UserInput

type RawInput = Event SDL.EventPayload -- SDL events
type MousePos = (Double, Double)

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
parseRawInput :: SF RawInput AppInput
parseRawInput = accumHoldBy nextAppInput initAppInput

extractMousePos :: Integral b => Point V2 b -> (Double, Double)
extractMousePos (P (V2 x y)) = (fromIntegral x, fromIntegral y)

nextAppInput :: AppInput -> SDL.EventPayload -> AppInput
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
  case (inpMouseLeft appInput, inpMouseRight appInput) of
   (Just _, Just _) -> appInput {inpMouseLeft = mousePos, inpMouseRight = mousePos}
   (Nothing, Just _) -> appInput {inpMouseRight = mousePos}
   (Just _, Nothing) -> appInput {inpMouseLeft = mousePos}
   _ -> appInput
  where mousePos = Just $ extractMousePos (SDL.mouseMotionEventPos ev)
nextAppInput appInput (SDL.KeyboardEvent ev) =
  let motion = SDL.keyboardEventKeyMotion ev in
    case scancode ev of
        SDL.ScancodeEscape -> appInput { inpQuit = True }
        SDL.ScancodeC -> appInput { inpClear = motion == SDL.Pressed }
        SDL.ScancodeT -> appInput { inpToggleWater = motion == SDL.Pressed }
        _ -> appInput { inpClear = False } -- could put more if want to.
    where scancode = SDL.keysymScancode . SDL.keyboardEventKeysym
nextAppInput appInput _ = appInput
