module Reactivity.MouseInput
  ( MouseInput(..)
  , getMouseInput
  , initialMouse
  ) where
import Debug.Trace

-- the "scaled" mouse input (scaled to the position of the canvas, not the screen)
data MouseInput = MouseInput
  { mouseDown    :: !Bool
  , mouseRightDown :: !Bool
  , mousePosLast :: !(Int, Int)
  , mousePos     :: !(Int, Int)
  } deriving (Show)

type WinSize = (Int, Int)

type CanvasSizeN = Int

canvasPosFromScreen :: Int -> (Int, Int) -> (Int, Int) -> (Int, Int)
canvasPosFromScreen n (width, height) (x, y) =
  (truncate (dx / dw * dn), truncate (dy / dh * dn))
  where
    dx = fromIntegral x :: Double
    dy = fromIntegral y :: Double
    dn = fromIntegral n :: Double
    dw = fromIntegral width :: Double
    dh = fromIntegral height :: Double

getMouseInput ::
     Bool -> ((Int, Int), CanvasSizeN, WinSize) -> MouseInput -> MouseInput
getMouseInput isDown canvasInfo@(screenPosCurrent, n, windowDims) prevMouseInput =
  let scaledCurrentPos = canvasPosFromScreen n windowDims screenPosCurrent
   in if not (mouseDown prevMouseInput || mouseRightDown prevMouseInput) && isDown
        then MouseInput
               { mouseDown = isDown
               , mousePos = scaledCurrentPos
               , mousePosLast = scaledCurrentPos
               , mouseRightDown = False
               }
        else prevMouseInput
               { mouseDown = isDown
               , mousePos = scaledCurrentPos
               , mousePosLast = mousePos prevMouseInput
               }

initialMouse :: MouseInput
initialMouse =
  MouseInput {mouseDown = False, mousePosLast = (0, 0), mousePos = (0, 0), mouseRightDown = False}