module Interaction.Input
  ( WinInput,
  AppInput(..)
  ) where

-- taken mostly from the SDL/Yampa Mandelbrot tutorial.

import           FRP.Yampa
import qualified SDL
import SDL.Vect (Point(P))
import           Linear (V2(..))

type WinInput = Event SDL.EventPayload -- SDL events

-- the AppInput: i.e. the input we actually care about
data AppInput = AppInput
  { inpMousePos    :: (Double, Double) -- ^ Current mouse position
  , inpMouseLeft   :: Maybe (Double, Double) -- ^ Down button currently down
  , inpMouseRight  :: Maybe (Double, Double) -- ^ Right button currently down
  , inpQuit        :: Bool -- ^ SDL's QuitEvent
  , inpKeyPressed  :: Maybe SDL.Scancode
  , inpKeyReleased :: Maybe SDL.Scancode
  }

initAppInput :: AppInput
initAppInput =
  AppInput
    { inpMousePos = (0, 0)
    , inpMouseLeft = Nothing
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
        newAppInput = appInput {inpMousePos = mousePos}
        in case (button, motion) of
            (SDL.ButtonLeft, SDL.Released) -> newAppInput {inpMouseLeft = Nothing}
            (SDL.ButtonRight, SDL.Released) -> newAppInput {inpMouseRight = Just mousePos}
            (SDL.ButtonLeft, SDL.Pressed) -> newAppInput {inpMouseLeft = Nothing}
            (SDL.ButtonRight, SDL.Pressed) -> newAppInput {inpMouseRight = Just mousePos}
nextAppInput appInput (SDL.MouseMotionEvent ev) = 
    appInput {inpMousePos = extractMousePos (SDL.mouseMotionEventPos ev)}
nextAppInput appInput (SDL.KeyboardEvent ev) =
    case scancode ev of
        SDL.ScancodeEscape -> appInput { inpQuit = True }
        _ -> appInput -- could put more if want to.
    where scancode = SDL.keysymScancode . SDL.keyboardEventKeysym
