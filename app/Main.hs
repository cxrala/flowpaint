{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Monad       (forM_)
import           Data.Word           (Word32)
import           Graphics.UI.GLUT    as G
import           Linear              (Additive (zero), V4 (..))

import           Data.IORef          (IORef, modifyIORef, newIORef, readIORef,
                                      writeIORef)

import           SDL                 (KeyModifier)

import           Data.Int            (Int32)
import           System.Exit         (ExitCode (ExitSuccess), exitSuccess,
                                      exitWith)
import qualified Utils.Matrix        as M
import           Utils.Matrix        (matrixNeighbours)

import           Data.Ord            (clamp)
import           Interface.Canvas    (Canvas (..), dimsFromN)
import           Interface.UserInput (MouseInput, getDuplicateMouseInput,
                                      getMouseInput, initialMouse)
import           Simulation.Source
import           Simulation.State    (State (surfaceLayerDensity), initialState,
                                      nextState)

-- import Simulation.State
canvas :: Canvas
canvas = Canvas {canvasScreen = (900, 900), canvasN = 50}

dims :: (Int, Int)
dims = dimsFromN (canvasN canvas)

colorVertex :: Color3 GLfloat -> Vertex2 GLfloat -> IO ()
colorVertex c v = do
  color c
  vertex v

getQuadDens :: (Int, Int) -> M.Matrix Double -> [GLfloat]
getQuadDens p@(x, y) m =
  map
    (realToFrac . (`M.matrixGet` m))
    [p, (x + 1, y), (x + 1, y + 1), (x, y + 1)]

drawDensity :: M.Matrix Double -> IO ()
drawDensity m = do
  color (Color3 (1 :: GLfloat) 0 1)
  let h = 2.0 / fromIntegral (canvasN canvas)
  let f i = (fromIntegral i :: GLfloat) * h - 1.0 - h
  renderPrimitive G.Quads
    $ forM_
        [(x, y) | x <- [1 .. canvasN canvas], y <- [1 .. canvasN canvas]]
        (\(i, j) -> do
           let [d00, d10, d11, d01] = getQuadDens (i, j) m
           let c x = 1 - clamp (0, 1) x
           colorVertex (Color3 (c d00) (c d00) 1) $ Vertex2 (f i) (f j)
           colorVertex (Color3 (c d00) (c d00) 1) $ Vertex2 (f i + h) (f j)
           colorVertex (Color3 (c d00) (c d00) 1) $ Vertex2 (f i + h) (f j + h)
           colorVertex (Color3 (c d00) (c d00) 1) $ Vertex2 (f i) (f j + h))
  flush

displayFunc :: IORef State -> DisplayCallback
displayFunc s = do
  clear [ColorBuffer]
  state <- readIORef s
  drawDensity $ surfaceLayerDensity state
  -- print $ waterDensity state
  swapBuffers

idleFunc :: IORef State -> IORef MouseInput -> IdleCallback
idleFunc sref iref = do
  input <- readIORef iref
  let src = getSourceFromMouseInput input dims
  modifyIORef sref (nextState src)
  postRedisplay Nothing

------- KEYBOARD INPUT AND HANDLING
setMouseData :: KeyState -> (Int, Int) -> (Int, Int) -> MouseInput
setMouseData km coords@(x, y) viewport =
  case km of
    Down -> getDuplicateMouseInput True (coords, canvas, viewport)
    Up   -> getDuplicateMouseInput False (coords, canvas, viewport)

setMouseMovedData :: (Int, Int) -> (Int, Int) -> MouseInput -> MouseInput
setMouseMovedData currPos viewport =
  getMouseInput True (currPos, canvas, viewport)

keyMouseFunc :: IORef MouseInput -> IORef State -> KeyboardMouseCallback
keyMouseFunc _ _ (Char 'q') _ _ _ = exitSuccess
keyMouseFunc _ s (Char 'c') _ _ _ = writeIORef s (initialState dims)
keyMouseFunc i _ _ km _ (Position x y) = do
  (_, Size width height) <- G.get viewport
  writeIORef i
    $ setMouseData
        km
        (fromIntegral x :: Int, fromIntegral y :: Int)
        (fromIntegral width, fromIntegral height)

mouseMotionFunc :: IORef MouseInput -> MotionCallback
mouseMotionFunc i (Position x y) = do
  (_, Size width height) <- G.get viewport
  input <- readIORef i
  modifyIORef i
    $ setMouseMovedData
        (fromIntegral x, fromIntegral y)
        (fromIntegral width, fromIntegral height)

-- This just starts up the event loop
main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBAMode]
  let (screenWidth, screenHeight) = canvasScreen canvas
  initialWindowSize
    $= Size (fromIntegral screenWidth) (fromIntegral screenHeight)
  initialWindowPosition $= Position 0 0
  _ <- G.createWindow "flowpaint"
  stateRef <- newIORef (initialState dims)
  input <- newIORef initialMouse
  displayCallback $= displayFunc stateRef
  keyboardMouseCallback $= Just (keyMouseFunc input stateRef)
  idleCallback $= Just (idleFunc stateRef input)
  motionCallback $= Just (mouseMotionFunc input)
  mainLoop
