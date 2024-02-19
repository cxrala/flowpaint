{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           SDL
import Control.Monad
import Graphics.UI.GLUT (Key(MouseButton))

import FRP.Yampa


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


main :: IO ()
main = do
  initializeAll
  window <- createWindow "flowpaint" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer
  destroyWindow window