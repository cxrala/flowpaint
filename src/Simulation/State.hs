{-# LANGUAGE BangPatterns #-}

module Simulation.State
  ( State(..)
  , nextState
  , initialState
  , dimsFromN
  , getSurfaceLayer
  ) where

import           Data.Maybe                 (fromMaybe)
import           Debug.Trace                (trace)
import           Simulation.Capillary
import           Simulation.Colors
import           Simulation.Source          (Source)
import           Simulation.SurfaceLayer
import           Simulation.VelocityField
import           Simulation.WaterQuantities
import           Utils.Fields
import           Utils.Matrix

--- pigment layer, and its corresponding functions.
data PigmentLayers = PigmentLayers
  { surfaceLayerDensitiesRYB :: !(ScalarField, ScalarField, ScalarField)
  , pigmentDensitiesRYB      :: !(ScalarField, ScalarField, ScalarField)
  } deriving (Show)

getColorLayer :: (PigmentLayers -> (a, a, a)) -> Color -> State -> a
getColorLayer f color state =
  let (r, y, b) = f $ pigLayers state
   in case color of
        Red    -> r
        Yellow -> y
        Blue   -> b

getPigmentLayer :: Color -> State -> ScalarField
getPigmentLayer = getColorLayer pigmentDensitiesRYB

getSurfaceLayer :: Color -> State -> ScalarField
getSurfaceLayer = getColorLayer surfaceLayerDensitiesRYB

updateColorLayer ::
     Color -> State -> ScalarField -> (ScalarField, ScalarField, ScalarField)
updateColorLayer color state newVal =
  let (r, y, b) = surfaceLayerDensitiesRYB $ pigLayers state
   in case color of
        Red    -> (newVal, y, b)
        Yellow -> (r, newVal, b)
        Blue   -> (r, y, newVal)

updateSurfaceLayer :: Color -> State -> ScalarField -> PigmentLayers
updateSurfaceLayer color prevState newVal =
  let oldPigLayers = pigLayers prevState
   in oldPigLayers
        {surfaceLayerDensitiesRYB = updateColorLayer color prevState newVal}

updatePigmentLayer :: Color -> State -> ScalarField -> PigmentLayers
updatePigmentLayer color prevState newVal =
  let oldPigLayers = pigLayers prevState
   in oldPigLayers
        {pigmentDensitiesRYB = updateColorLayer color prevState newVal}

stepPigmentLayers :: State -> Matrix Double -> Double -> PigmentLayers
stepPigmentLayers state hmap dt =
  let pigLayer x = getPigmentLayer x state
      surfaceLayer x = getSurfaceLayer x state
      updatePigs color =
        calculateSurfaceLayer
          (pigLayer color)
          (surfaceLayer color)
          (pigLayer color)
          hmap
          dt
      !(newPigR, newSurR) = updatePigs Red
      !(newPigY, newSurY) = updatePigs Yellow
      !(newPigB, newSurB) = updatePigs Blue
      -- normalisedSurfaceLayers = normaliseSurfaceLayers (newSurR, newSurY, newSurB)
   in PigmentLayers
        { surfaceLayerDensitiesRYB = (newSurR, newSurY, newSurB) -- normalisedSurfaceLayers
        , pigmentDensitiesRYB = (newPigR, newPigY, newPigB)
        }

stepPigments :: State -> Maybe Source -> Matrix Double -> VelocityField -> Double -> Int -> (ScalarField, ScalarField, ScalarField)
stepPigments !prevState !mSrc !hmap !vstep !dt !n =
  let (r, y, b) = pigmentDensitiesRYB $ pigLayers prevState
      newPigmentLayer src color =
        updateDens
          (getPigmentLayer color prevState)
          src
          hmap
          vstep
          (mask prevState)
          dt
          n
      !defaultLayer = newPigmentLayer (zeroMatrix $ canvasDims prevState)
   in case mSrc of
        Nothing ->
          ( defaultLayer Red
          , defaultLayer Yellow
          , defaultLayer Blue)
        Just (src, color) ->
          case color of
            Red ->
              ( newPigmentLayer src Red
              , defaultLayer Yellow
              , defaultLayer Blue)
            Yellow ->
              ( defaultLayer Red
              , newPigmentLayer src Yellow
              , defaultLayer Blue)
            Blue ->
              ( defaultLayer Red
              , defaultLayer Yellow
              , newPigmentLayer src Blue)

---- rest of the state
data State = State
  { velocityField         :: !VelocityField -- the fluid velocity on top of the canvas. (varies)
  , waterDensity          :: !ScalarField -- the amount of water on top of the canvas. (varies)
  , pigLayers             :: PigmentLayers -- the different pigment layers on the canvas (varies)
  , capillaryLayerDensity :: !ScalarField -- the density of water inside the capillary layer (varies)
  , heightMap             :: !ScalarField -- the heightmap (CONSTANT)
  , mask                  :: !(Matrix Bool) -- the boundaries where the stroke is respected (varies)
  , constants             :: !PhysicsConstants -- the constants (CONSTANT)
  , canvasSize            :: !Int -- the size of the canvas (CONSTANT)
  , canvasDims            :: !(Int, Int) -- the dimensions of the canvas (CONSTANT)
  }

data PhysicsConstants = PhysicsConstants
  { physConstantDt     :: !Double -- simulation speed
  , physConstantDiff   :: !Double -- diffusion rate
  , physConstantVisc   :: !Double -- viscosity
  , physConstantSource :: !Double -- density deposited
  }

zeroMatrix dims = matrixInit dims 0

initialState :: Int -> State
initialState sizeN =
  let dims = dimsFromN sizeN
      initialMat = zeroMatrix dims
   in State
        { velocityField = (initialMat, initialMat)
        , waterDensity = initialMat
        , pigLayers =
            PigmentLayers
              { surfaceLayerDensitiesRYB = (initialMat, initialMat, initialMat)
              , pigmentDensitiesRYB = (initialMat, initialMat, initialMat)
              }
        , capillaryLayerDensity = initialMat
        , heightMap = getWorleyNoise (genPoints 3 dims) dims
        , mask = matrixInit dims False
        , constants =
            PhysicsConstants
              { physConstantDt = 0.5
              , physConstantDiff = 0.0001
              , physConstantVisc = 0.0003
              , physConstantSource = 1000.0
              }
        , canvasSize = sizeN
        , canvasDims = dims
        }

dimsFromN :: Int -> (Int, Int)
dimsFromN n = (n + 2, n + 2)

-- propogating to the next state
-- exported
nextState :: Maybe Source -> State -> State
nextState = step

-- private
step :: Maybe Source -> State -> State
step mSrc !prevState
  -- TODO: change so that there is absolutely no addSource (ie just diffusion)
      -- initialise constants
 =
  let n = canvasSize prevState
      constantproperties = constants prevState
      dt = physConstantDt constantproperties
      diff = physConstantDiff constantproperties
      visc = physConstantVisc constantproperties
      zeroGrid = zeroMatrix (matrixDims $ waterDensity prevState)
      hmap = heightMap prevState
      capillary = capillaryLayerDensity prevState
      src =
        case mSrc of
          Nothing     -> zeroMatrix $ canvasDims prevState
          Just (m, _) -> m
      -- state updates
      !vstep = velStep n (velocityField prevState) (zeroGrid, zeroGrid) visc dt -- velocity after one step
      !(vField, dField) =
        updateWater
          (waterDensity prevState)
          src
          hmap
          vstep
          (mask prevState)
          dt
          n
      -- pigment updates
      !sourceUpdatedPigLayer = (pigLayers prevState) { pigmentDensitiesRYB = stepPigments prevState mSrc hmap vstep dt n}
      !newPigLayers =
        stepPigmentLayers
          (prevState {pigLayers = sourceUpdatedPigLayer})
          hmap
          dt
      -- capillary updates
      (newCapillary, newShallowFluid, newMask) =
        simulateCapillaryFlow capillary dField hmap n diff dt
   in prevState
        { waterDensity = dField
        , velocityField = vField
        , pigLayers = newPigLayers
        , capillaryLayerDensity = newCapillary
        , heightMap = heightMap prevState
        , mask = newMask
        , constants = constantproperties
        }
