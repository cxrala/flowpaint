{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.Word                (Word32)
import           Graphics.UI.GLUT         as G
import           Linear                   (V4 (..))

import           Data.IORef

import           SDL                      (KeyModifier)
-- import           SDL
import           Simulation.VelocityField (densStep, velStep)
import           System.Exit              (ExitCode (ExitSuccess), exitSuccess,
                                           exitWith)
import qualified Utils.Matrix             as M
import Data.Int (Int32)

screenWidth :: Int32
screenHeight :: Int32
screensize@(screenWidth, screenHeight) = (fromIntegral n, fromIntegral n)

-- grid size
n :: Int
n = 40

-- time step
dt :: Double
dt = 0.1

-- diffusion rate
diff :: Double
diff = 0.0001

-- |Viscosity of the fluid
visc :: Double
visc = 5

-- |Scales the mouse movement that generates a force
force :: Double
force = 5.0

-- |Amount of density that will be deposited
source :: Double
source = 100.0

data State = State
  { densityField  :: M.Matrix Double
  , velocityField :: (M.Matrix Double, M.Matrix Double)
  }

data Input = Input
  { mouseDown    :: Bool
  , mousePosLast :: (Int, Int)
  , mousePos     :: (Int, Int)
  }

initialState =
  State
    { densityField = M.matrixInit (n + 2, n + 2) 0
    , velocityField =
        (M.matrixInit (n + 2, n + 2) 0, M.matrixInit (n + 2, n + 2) 0)
    }

initialInput =
  Input {mouseDown = False, mousePosLast = (0, 0), mousePos = (0, 0)}

colorVertex :: Color3 GLfloat -> Vertex2 GLfloat -> IO ()
colorVertex c v = do
  color c
  vertex v

getQuadDens :: (Int, Int) -> M.Matrix Double -> [GLfloat]
getQuadDens p@(x, y) m = map (realToFrac . (`M.matrixGet` m)) [p, (x + 1, y), (x + 1, y + 1), (x, y + 1)]

drawDensity :: M.Matrix Double -> IO ()
drawDensity m = do
  color (Color3 (1 :: GLfloat) 0 1)
  let h = 2.0 / fromIntegral n
  let f i = (fromIntegral i :: GLfloat) * h - 1.0 - h
  renderPrimitive G.Quads
    $ forM_
        [(x, y) | x <- [1 .. n], y <- [1 .. n]]
        (\(i, j) -> do
           let [d00, d01, d10, d11] = getQuadDens (i, j) m
           colorVertex (Color3 d00 d00 d00) $ Vertex2 (f i) (f j)
           colorVertex (Color3 d00 d00 d00) $ Vertex2 (f i + h) (f j)
           colorVertex (Color3 d00 d00 d00) $ Vertex2 (f i + h) (f j + h)
           colorVertex (Color3 d00 d00 d00) $ Vertex2 (f i) (f j + h))
  flush

displayFunc :: IORef State -> DisplayCallback
displayFunc s = do
  clear [ColorBuffer]
  state <- readIORef s
  drawDensity $ densityField state
  swapBuffers

pos :: Int -> (Int, Int) -> (Int, Int) -> (Int, Int)
pos n (width,height) (x,y) = (truncate (dx/dw*dn), n - truncate (dy/dh*dn)) where
    dx = fromIntegral x :: Double
    dy = fromIntegral y :: Double
    dn = fromIntegral n :: Double 
    dw = fromIntegral width :: Double
    dh = fromIntegral height :: Double

------- IDLE MOVEMENT!
updateStateFromUI :: IORef Input -> IO (M.Matrix Double)
updateStateFromUI iref = do
  input <- readIORef iref
  (_, Size width height) <- G.get viewport
  let mousepos = mousePos input
  let scaledMousePos = pos n (fromIntegral width, fromIntegral height) mousepos 
  print mousepos
  return
    $ M.matrixGenerate
        (n + 2, n + 2)
        (\x ->
           if x == scaledMousePos
             then 1
             else 0)

zeroGrid :: M.Matrix Double
zeroGrid = M.matrixInit (n + 2, n + 2) 0

idleFunc :: IORef State -> IORef Input -> IdleCallback
idleFunc sref iref = do
  input <- readIORef iref
  state <- readIORef sref
  let (u, v) = velocityField state
  let dens = densityField state
  -- If necessary, update the prev values
  src <-
    if mouseDown input
      then updateStateFromUI iref
      else return zeroGrid
  writeIORef
    sref
    state
      { velocityField = velStep n u v zeroGrid zeroGrid visc dt
      , densityField = densStep n dens src u v diff dt
      }
  postRedisplay Nothing
  return ()

------- KEYBOARD INPUT AND HANDLING
setMouseData :: KeyState -> (Int, Int) -> Input
setMouseData km coords@(x, y) =
  case km of
    Down -> Input True coords coords
    Up   -> Input False coords coords

setMouseMovedData :: (Int, Int) -> Input -> Input
setMouseMovedData currPos ilast = ilast {mousePosLast = mousePos ilast, mousePos = currPos}

keyMouseFunc :: IORef Input -> IORef State -> KeyboardMouseCallback
keyMouseFunc _ _ (Char 'q') _ _ _ = exitSuccess
keyMouseFunc _ s (Char 'c') _ _ _ = writeIORef s initialState
keyMouseFunc i _ _ km _ (Position x y) =
  writeIORef i $ setMouseData km (fromIntegral x :: Int, fromIntegral y :: Int)

mouseMotionFunc :: IORef Input -> MotionCallback
mouseMotionFunc i (Position x y) = modifyIORef i $ setMouseMovedData (fromIntegral x, fromIntegral y)

-- This just starts up the event loop
main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBAMode]
  initialWindowSize $= Size (screenWidth * 4) (screenHeight * 4)
  initialWindowPosition $= Position 0 0
  _ <- G.createWindow "Barely Functional Fluid Dynamics"
  clearColor $= Color4 0 0 0 1
  state <- newIORef initialState
  input <- newIORef initialInput
  displayCallback $= displayFunc state
  keyboardMouseCallback $= Just (keyMouseFunc input state)
  idleCallback $= Just (idleFunc state input)
  motionCallback $= Just (mouseMotionFunc input)
  mainLoop
-- main :: IO ()
-- main = do
--   initializeAll
--   window <- createWindow "My SDL Application" defaultWindow
--   renderer <- createRenderer window (-1) defaultRenderer
--   appLoop renderer
--   destroyWindow window
-- appLoop :: Renderer -> IO ()
-- appLoop renderer = do
--   events <- pollEvents
--   let eventIsQPress event =
--         case eventPayload event of
--           KeyboardEvent keyboardEvent ->
--             keyboardEventKeyMotion keyboardEvent == Pressed
--               && keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
--           _ -> False
--       qPressed = any eventIsQPress events
--   rendererDrawColor renderer $= V4 0 0 0 0
--   clear renderer
--   present renderer
--   unless qPressed (appLoop renderer)
