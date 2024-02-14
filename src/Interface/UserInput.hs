module Interface.UserInput (
    MouseInput,
    mouseDown,
    mousePos,
    mousePosLast
) where

import Interface.Canvas

-- the "scaled" mouse input (scaled to the position of the canvas, not the screen)
data MouseInput = MouseInput
  { mouseDown    :: Bool
  , mousePosLast :: (Int, Int)
  , mousePos     :: (Int, Int)
  } deriving (Show)

canvasPosFromScreen :: Int -> (Int, Int) -> (Int, Int) -> (Int, Int)
canvasPosFromScreen n (width, height) (x, y) =
  (truncate (dx / dw * dn) + 1, n - truncate (dy / dh * dn))
  where
    dx = fromIntegral x :: Double
    dy = fromIntegral y :: Double
    dn = fromIntegral n :: Double
    dw = fromIntegral width :: Double
    dh = fromIntegral height :: Double

getMouseInput :: Bool -> ((Int, Int), Canvas) -> MouseInput -> MouseInput
getMouseInput isDown (screenPosCurrent, canvas) prevMouseInput =
    let prevMousePos = mousePosLast prevMouseInput in
        prevMouseInput {
            mouseDown = isDown,
            mousePos = canvasPosFromScreen (canvasN canvas) (canvasScreen canvas) screenPosCurrent,
            mousePosLast = mousePosLast prevMouseInput
        }