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

canvas :: Canvas
canvas = Canvas {canvasScreen = (900, 900), canvasN = 50}

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



------- animate -------

initialise :: IO (Event EventPayload)
initialise = return NoEvent

main :: IO ()
main = do
  initializeAll
  window <- createWindow "flowpaint" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  initTime <- newMVar =<< SDL.time
  let state = initialState
  (program, texture) <- initResources state 
  -- appLoop renderer
  let actuate = renderState 
  reactimate initialise (senseInput initTime) actuate sf
  destroyWindow window