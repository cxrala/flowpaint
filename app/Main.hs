{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import System.IO
import           Control.Monad
import           Graphics.UI.GLUT           (Key (MouseButton))
import           Interface.Render
import           Interface.Sense
import           Reactivity.Input
import           SDL                        hiding (Event)

import           Control.Concurrent         (newMVar)
import           Data.Text                  (Text)
import           Foreign.C                  (CInt)
import           FRP.Yampa
import           Reactivity.MouseInput      (initialMouse)
import           Reactivity.SignalFunctions
import           SDL.Raw                    (glSetAttribute)
import           SDL.Raw.Enum
import           Simulation.State
import GHC.IO.IOMode (IOMode(AppendMode, WriteMode))
import Data.IORef (newIORef, readIORef)
import Control.Exception (handle)
import SDL (EventPayload, WindowShownEventData)
import Codec.Serialise (writeFileSerialise)

openWindow :: Text -> (Int, Int) -> IO SDL.Window
openWindow title (sizex, sizey) = do
  SDL.initializeAll
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do
    renderQuality <- SDL.get SDL.HintRenderScaleQuality
    when (renderQuality /= SDL.ScaleLinear)
      $ putStrLn "Warning: Linear texture filtering not enabled!"
  let config =
        OpenGLConfig
          { glColorPrecision = V4 8 8 8 0
          , glDepthPrecision = 24
          , glStencilPrecision = 8
          , glMultisampleSamples = 4
          , glProfile = Core Normal 4 1
          }
  window <-
    SDL.createWindow
      title
      SDL.defaultWindow
        { SDL.windowInitialSize = V2 (fromIntegral sizex) (fromIntegral sizey)
        , SDL.windowGraphicsContext = OpenGLContext config
        }
  SDL.showWindow window
  _ <- SDL.glCreateContext window
  return window

closeWindow :: SDL.Window -> SDL.Texture -> IO ()
closeWindow window texture = do
  SDL.destroyTexture texture
  SDL.destroyWindow window
  SDL.quit

------- animate -------
initAnimation :: V2 CInt -> State -> IO (Event RawInput, (Int, Int))
initAnimation (V2 x y) state =
  return (NoEvent, (fromIntegral x, fromIntegral y))

main :: IO ()
main = do
  let windowDims = (700, 700)
  window <- openWindow "flowpaint" windowDims
  renderer <- SDL.createRenderer window (-1) defaultRenderer
  initTime <- newMVar =<< SDL.time
  let varWinSize = windowSize window
  currWinSize <- get varWinSize
  let canvasSize = 70
  let initState = initialState canvasSize
  let initMouse = initialMouse
  texture <- initResources initState renderer
  let initialise = initAnimation currWinSize initState
  let fileName = "./tests/TestStrokes/wetting_behaviour_toggle.txt"
  eventList <- newIORef []

  let sense = senseInput initTime eventList varWinSize
  let actuate = renderState (texture, renderer)
  let sf = signalFunction initState initMouse
  reactimate initialise sense actuate sf

  lst <- readIORef eventList
  writeFileSerialise fileName lst
  closeWindow window texture
