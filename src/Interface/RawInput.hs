{-# LANGUAGE DeriveGeneric #-}
module Interface.RawInput where
import GHC.Generics
import SDL.Vect (Point(P))
import           Linear (V2(..))
import Data.Int (Int32)
import qualified SDL
import Codec.Serialise

-- an easily serialisable wrapper around SDL2 events.

data MouseButton = ButtonLeft | ButtonRight deriving (Generic, Eq, Show)
data InputMotion = Released | Pressed deriving (Generic, Eq, Show)
data Scancode = ScancodeEscape | ScancodeC | ScancodeT | ScancodeOther deriving (Generic, Eq, Show)
instance Serialise MouseButton
instance Serialise InputMotion
instance Serialise Scancode

data MouseButtonEventData = MkMouseButtonEvent {
    mouseButtonEventButton :: MouseButton,
    mouseButtonEventMotion :: InputMotion,
    mouseButtonEventPos :: (Int32, Int32)
} deriving (Generic, Eq, Show)
instance Serialise MouseButtonEventData

newtype MouseMotionEventData = MkMouseMotionEvent {
    mouseMotionEventPos :: (Int32, Int32)
} deriving (Generic, Eq, Show)
instance Serialise MouseMotionEventData

data KeyboardEventData = MkKeyboardEvent {
    keyboardEventKeyMotion :: InputMotion,
    keyboardScancode :: Scancode
} deriving (Generic, Eq, Show)
instance Serialise KeyboardEventData

data RawInput = MouseButtonEvent MouseButtonEventData | MouseMotionEvent MouseMotionEventData | KeyboardEvent KeyboardEventData | UnknownEvent deriving (Generic, Eq, Show)
instance Serialise RawInput

parseEventPayload :: SDL.EventPayload -> RawInput
parseEventPayload eventPayload =
    case eventPayload of
        SDL.MouseButtonEvent ev -> 
            MouseButtonEvent $ MkMouseButtonEvent {
                mouseButtonEventButton = parseMouseButton $ SDL.mouseButtonEventButton ev,
                mouseButtonEventMotion = parseInputMotion $ SDL.mouseButtonEventMotion ev,
                mouseButtonEventPos = extractMousePos $ SDL.mouseButtonEventPos ev
            }
        SDL.MouseMotionEvent ev ->
            MouseMotionEvent . MkMouseMotionEvent . extractMousePos $ SDL.mouseMotionEventPos ev
        SDL.KeyboardEvent ev ->
            KeyboardEvent $ MkKeyboardEvent {
                keyboardEventKeyMotion = parseInputMotion $ SDL.keyboardEventKeyMotion ev,
                keyboardScancode = parseScancode . SDL.keysymScancode $ SDL.keyboardEventKeysym ev
            }
        _ -> UnknownEvent
    where
        parseInputMotion SDL.Released = Released
        parseInputMotion SDL.Pressed = Pressed
        parseMouseButton SDL.ButtonLeft = ButtonLeft
        parseMouseButton SDL.ButtonRight = ButtonRight
        parseMouseButton _ = ButtonLeft
        parseScancode SDL.ScancodeEscape = ScancodeEscape
        parseScancode SDL.ScancodeC = ScancodeC
        parseScancode SDL.ScancodeT = ScancodeT
        parseScancode _ = ScancodeOther
        extractMousePos (P (V2 x y)) = (fromIntegral x, fromIntegral y)