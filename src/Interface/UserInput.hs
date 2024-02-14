module Interface.UserInput
  ( MouseInput(..)
  , getMouseInput
  , initialMouse
  , getDuplicateMouseInput
  ) where

import           Interface.Canvas

-- the "scaled" mouse input (scaled to the position of the canvas, not the screen)
data MouseInput = MouseInput
  { mouseDown    :: Bool
  , mousePosLast :: (Int, Int)
  , mousePos     :: (Int, Int)
  } deriving (Show)

type ScaledCanvas = (Int, Int)

canvasPosFromScreen :: Int -> (Int, Int) -> (Int, Int) -> (Int, Int)
canvasPosFromScreen n (width, height) (x, y) =
  (truncate (dx / dw * dn) + 1, n - truncate (dy / dh * dn))
  where
    dx = fromIntegral x :: Double
    dy = fromIntegral y :: Double
    dn = fromIntegral n :: Double
    dw = fromIntegral width :: Double
    dh = fromIntegral height :: Double

getMouseInput ::
     Bool -> ((Int, Int), Canvas, ScaledCanvas) -> MouseInput -> MouseInput
getMouseInput isDown (screenPosCurrent, canvas, scaling) prevMouseInput =
  prevMouseInput
    { mouseDown = isDown
    , mousePos = canvasPosFromScreen (canvasN canvas) scaling screenPosCurrent
    , mousePosLast = mousePos prevMouseInput
    }

initialMouse :: MouseInput
initialMouse =
  MouseInput {mouseDown = False, mousePosLast = (0, 0), mousePos = (0, 0)}

getDuplicateMouseInput ::
     Bool -> ((Int, Int), Canvas, ScaledCanvas) -> MouseInput
getDuplicateMouseInput isDown canvasInfo =
  let mouseFirst = getMouseInput isDown canvasInfo initialMouse
   in getMouseInput isDown canvasInfo mouseFirst
