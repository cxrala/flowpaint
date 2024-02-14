{-# LANGUAGE OverloadedStrings #-}

module Main (
  main
) where

import           Control.Monad
import           Data.Word                  (Word32)
import           Graphics.UI.GLUT           as G
import           Linear                     (Additive (zero), V4 (..))

import           Data.IORef

import           SDL                        (KeyModifier)

import           Data.Int                   (Int32)
import           Data.List                  (sort, unfoldr)

-- import           SDL
import           Simulation.VelocityField   (densStep, velStep)
import           Simulation.WaterQuantities (updateWater)

import           Data.Ord                   (clamp)
import           Data.Tuple
import           Simulation.Capillary       (genPoints, getWorleyNoise, simulateCapillaryFlow)
import           Simulation.SurfaceLayer    (calculateSurfaceLayer)
import           System.Exit                (ExitCode (ExitSuccess),
                                             exitSuccess, exitWith)
import qualified Utils.Matrix               as M
import           Utils.Matrix               (matrixNeighbours)

screenWidth :: Int32
screenHeight :: Int32
screensize@(screenWidth, screenHeight) = (900, 900)

-- grid size
n :: Int

n = 50

dims = (n + 2, n + 2)

-- time step
dt :: Double
dt = 0.5

-- diffusion rate
diff :: Double
diff = 0.0001

-- |Viscosity of the fluid
visc :: Double
visc = 0.0003

-- |Scales the mouse movement that generates a force
force :: Double
force = 5.0

-- |Amount of density that will be deposited
source :: Double
source = 1000.0

data State = State
  { waterDensity  :: M.Matrix Double
  , pigmentField  :: M.Matrix Double
  , velocityField :: (M.Matrix Double, M.Matrix Double)
  , surfaceLayer  :: M.Matrix Double
  , heightMap     :: M.Matrix Double
  , capillaryLayer :: M.Matrix Double
  , mask          :: M.Matrix Bool
  }

data Input = Input
  { mouseDown    :: Bool
  , mousePosLast :: (Int, Int)
  , mousePos     :: (Int, Int)
  } deriving (Show)

initialState =
  State
    { waterDensity = M.matrixInit dims 0
    , pigmentField = M.matrixInit dims 0
    , velocityField = (M.matrixInit dims 0, M.matrixInit dims 0)
    , surfaceLayer = M.matrixInit dims 0
    , heightMap = getWorleyNoise (genPoints 3 dims) dims
    , capillaryLayer = M.matrixInit dims 0
    , mask = M.matrixInit dims False
    }

initialInput =
  Input {mouseDown = False, mousePosLast = (0, 0), mousePos = (0, 0)}

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
  let h = 2.0 / fromIntegral n
  let f i = (fromIntegral i :: GLfloat) * h - 1.0 - h
  renderPrimitive G.Quads
    $ forM_
        [(x, y) | x <- [1 .. n], y <- [1 .. n]]
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
  drawDensity $ surfaceLayer state
  -- print $ waterDensity state
  swapBuffers

pos :: Int -> (Int, Int) -> (Int, Int) -> (Int, Int)
pos n (width, height) (x, y) =
  (truncate (dx / dw * dn) + 1, n - truncate (dy / dh * dn))
  where
    dx = fromIntegral x :: Double
    dy = fromIntegral y :: Double
    dn = fromIntegral n :: Double
    dw = fromIntegral width :: Double
    dh = fromIntegral height :: Double

neighbours :: (Num a, Num b) => (a, b) -> [(a, b)]
neighbours (x, y) = [(x + 1, y), (x - 1, y), (x, y - 1), (x, y + 1)]

line :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
line pa@(xa, ya) pb@(xb, yb) = map maySwitch . unfoldr go $ (x1, y1, 0)
  where
    steep = abs (yb - ya) > abs (xb - xa)
    maySwitch =
      if steep
        then (\(x, y) -> (y, x))
        else id
    [(x1, y1), (x2, y2)] = sort [maySwitch pa, maySwitch pb]
    deltax = x2 - x1
    deltay = abs (y2 - y1)
    ystep =
      if y1 < y2
        then 1
        else -1
    go (xTemp, yTemp, error)
      | xTemp > x2 = Nothing
      | otherwise = Just ((xTemp, yTemp), (xTemp + 1, newY, newError))
      where
        tempError = error + deltay
        (newY, newError) =
          if (2 * tempError) >= deltax
            then (yTemp + ystep, tempError - deltax)
            else (yTemp, tempError)

------- IDLE MOVEMENT!
updateStateFromUI :: IORef Input -> IO (M.Matrix Double)
updateStateFromUI iref = do
  input <- readIORef iref
  (_, Size width height) <- G.get viewport
  let mouseposprev = mousePosLast input
  let mousepos = mousePos input
  let scaledMousePosPrev =
        pos n (fromIntegral width, fromIntegral height) mouseposprev
  let scaledMousePos = pos n (fromIntegral width, fromIntegral height) mousepos
  -- print (scaledMousePos, scaledMousePosPrev)
  return
    $ M.matrixGenerate
        (n + 2, n + 2)
        (\x ->
           if x `elem` line scaledMousePosPrev scaledMousePos
             then 1
             else 0)

zeroGrid :: M.Matrix Double
zeroGrid = M.matrixInit (n + 2, n + 2) 0

idleFunc :: IORef State -> IORef Input -> IdleCallback
idleFunc sref iref = do
  input <- readIORef iref
  state <- readIORef sref
  let vfield = velocityField state
  let water = waterDensity state
  let surface = surfaceLayer state
  let hmap = heightMap state
  let capillary = capillaryLayer state
  let stroke = mask state
  -- If necessary, update the prev values
  src <-
    if mouseDown input
      then updateStateFromUI iref
      else return zeroGrid
  let vstep = velStep n vfield (zeroGrid, zeroGrid) visc dt
  let (vField, dField) = updateWater water src hmap vstep stroke dt n
  let (newPigmentLayer, newSurfaceLayer) =
        calculateSurfaceLayer dField surface dField hmap dt
  let (newCapillary, newShallowFluid, newMask) = simulateCapillaryFlow capillary dField hmap n diff dt
  writeIORef
    sref
    state
      { velocityField = vField
      , waterDensity = dField
      , pigmentField = newPigmentLayer
      , surfaceLayer = newSurfaceLayer
      , capillaryLayer = newCapillary
      , mask = newMask
      }
  postRedisplay Nothing

------- KEYBOARD INPUT AND HANDLING
setMouseData :: KeyState -> (Int, Int) -> Input
setMouseData km coords@(x, y) =
  case km of
    Down -> Input True coords coords
    Up   -> Input False coords coords

setMouseMovedData :: (Int, Int) -> Input -> Input
setMouseMovedData currPos ilast =
  ilast {mousePosLast = mousePos ilast, mousePos = currPos}

keyMouseFunc :: IORef Input -> IORef State -> KeyboardMouseCallback
keyMouseFunc _ _ (Char 'q') _ _ _ = exitSuccess
keyMouseFunc _ s (Char 'c') _ _ _ = writeIORef s initialState
keyMouseFunc i _ _ km _ (Position x y) =
  writeIORef i $ setMouseData km (fromIntegral x :: Int, fromIntegral y :: Int)

mouseMotionFunc :: IORef Input -> MotionCallback
mouseMotionFunc i (Position x y) =
  modifyIORef i $ setMouseMovedData (fromIntegral x, fromIntegral y)

-- This just starts up the event loop
main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBAMode]
  initialWindowSize $= Size screenWidth screenHeight
  initialWindowPosition $= Position 0 0
  _ <- G.createWindow "flowpaint"
  state <- newIORef initialState
  input <- newIORef initialInput
  displayCallback $= displayFunc state
  keyboardMouseCallback $= Just (keyMouseFunc input state)
  idleCallback $= Just (idleFunc state input)
  motionCallback $= Just (mouseMotionFunc input)
  mainLoop
