{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           SDL hiding (Event)
import Control.Monad
import Graphics.UI.GLUT (Key(MouseButton))
import Interaction.Input
import Interaction.Render
import Interaction.Sense
import Interaction.SignalFunctions
import FRP.Yampa
import Control.Concurrent (newMVar)
import Simulation.State
import Foreign.C (CInt)
import Data.Text (Text)
import SDL.Raw.Enum
import SDL.Raw (glSetAttribute)
import Interface.UserInput (initialMouse)


appLoop :: Renderer -> IO()
appLoop renderer = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events

  rendererDrawColor renderer $= V4 0 255 0 255
  drawPoint renderer (P (V2 3 3))
  present renderer
  unless qPressed (appLoop renderer)



openWindow :: Text -> (Int, Int) -> IO SDL.Window
openWindow title (sizex, sizey) = do
    SDL.initializeAll

    SDL.HintRenderScaleQuality $= SDL.ScaleLinear
    do renderQuality <- SDL.get SDL.HintRenderScaleQuality
       when (renderQuality /= SDL.ScaleLinear) $
         putStrLn "Warning: Linear texture filtering not enabled!"

    let config = OpenGLConfig { glColorPrecision = V4 8 8 8 0
                              , glDepthPrecision = 24
                              , glStencilPrecision = 8
                              , glMultisampleSamples = 4
                              , glProfile = Core Normal 4 1
                              }

    window <- SDL.createWindow
              title
              SDL.defaultWindow
              { SDL.windowInitialSize = V2 (fromIntegral sizex) (fromIntegral sizey)
              , SDL.windowGraphicsContext = OpenGLContext config }

    SDL.showWindow window
    _ <- SDL.glCreateContext window

    return window

closeWindow :: SDL.Window -> SDL.Texture -> IO ()
closeWindow window texture = do
    SDL.destroyTexture texture
    SDL.destroyWindow window
    SDL.quit

------- animate -------

initAnimation :: V2 CInt -> State -> IO (Event EventPayload, (Int, Int))
initAnimation (V2 x y) state = return (NoEvent, (fromIntegral x, fromIntegral y))

main :: IO ()
main = do
  let windowDims = (1000, 1000)
  window <- openWindow "flowpaint" windowDims
  renderer <- SDL.createRenderer window (-1) defaultRenderer
  initTime <- newMVar =<< SDL.time
  let varWinSize = windowSize window
  currWinSize <- get varWinSize
  let canvasSize = 75
  
  let initState = initialState canvasSize
  let initMouse = initialMouse
  
  texture <- initResources initState renderer
  let initialise = initAnimation currWinSize initState
  let sense = senseInput initTime varWinSize
  let actuate = renderState (texture, renderer)
  let sf = signalFunction initState initMouse

  reactimate initialise sense actuate sf
  closeWindow window texture