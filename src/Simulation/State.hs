{-# LANGUAGE BangPatterns #-}
module Simulation.State
  ( State(..)
  , nextState
  , initialState
  , dimsFromN
  ) where

import           Simulation.Capillary
import           Simulation.SurfaceLayer
import           Simulation.VelocityField
import           Simulation.WaterQuantities
import           Utils.Fields
import           Utils.Matrix
import Simulation.Source (Source)
import Data.Maybe (fromMaybe)

data State = State
  { velocityField         :: !VelocityField
  , waterDensity          :: !ScalarField
  , pigmentDensity        :: !ScalarField
  , surfaceLayerDensity   :: !ScalarField
  , capillaryLayerDensity :: !ScalarField
  , heightMap             :: !ScalarField
  , mask                  :: !(Matrix Bool)
  , constants             :: !PhysicsConstants
  , canvasSize      :: !Int
  , canvasDims :: !(Int, Int)
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
      intialMat = zeroMatrix dims
   in State
        { velocityField = (intialMat, intialMat)
        , waterDensity = intialMat
        , pigmentDensity = intialMat
        , surfaceLayerDensity = intialMat
        , capillaryLayerDensity = intialMat
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

nextState :: Maybe Source -> State -> State
nextState src prevState = step (fromMaybe (zeroMatrix $ canvasDims prevState) src) prevState

-- nextState src prevState = step (fromMaybe concreteZeroMatrix src) prevState
--   where concreteZeroMatrix = zeroMatrix . matrixDims $ waterDensity prevState

step :: Source -> State -> State
step !src !prevState =
  let n = canvasSize prevState
      constantproperties = constants prevState
      dt = physConstantDt constantproperties
      diff = physConstantDiff constantproperties
      visc = physConstantVisc constantproperties
      zeroGrid = zeroMatrix (matrixDims $ waterDensity prevState)
      vstep = velStep n (velocityField prevState) (zeroGrid, zeroGrid) visc dt
      !(vField, dField) = updateWater (waterDensity prevState) src (heightMap prevState) vstep (mask prevState) dt n
      (newPigmentLayer, newSurfaceLayer) =
        calculateSurfaceLayer dField (surfaceLayerDensity prevState) dField (heightMap prevState) dt
      (newCapillary, newShallowFluid, newMask) =
        simulateCapillaryFlow (capillaryLayerDensity prevState) dField (heightMap prevState) n diff dt
  in prevState { waterDensity = dField
      , velocityField = vField
      , pigmentDensity = newPigmentLayer
      , surfaceLayerDensity = newSurfaceLayer
      , capillaryLayerDensity = newCapillary
      , heightMap = heightMap prevState
      , mask = newMask
      , constants = constantproperties
      }
