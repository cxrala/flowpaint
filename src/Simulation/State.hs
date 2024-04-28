{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
module Simulation.State
  ( State(..)
  , PhysicsConstants
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
import GHC.Generics (Generic)

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
  } deriving (Show, Eq, Generic)


data PhysicsConstants = PhysicsConstants
  { physConstantDt     :: !Double -- simulation speed
  , physConstantDiff   :: !Double -- diffusion rate
  , physConstantVisc   :: !Double -- viscosity
  , physConstantSource :: !Double -- density deposited
  } deriving (Show, Eq, Generic)

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

nextState :: Bool -> Maybe Source -> State -> State
nextState isPigmentUpdate src prevState = step isPigmentUpdate (fromMaybe (zeroMatrix $ canvasDims prevState) src) prevState

-- nextState src prevState = step (fromMaybe concreteZeroMatrix src) prevState
--   where concreteZeroMatrix = zeroMatrix . matrixDims $ waterDensity prevState

step :: Bool -> Source -> State -> State
step isPigmentUpdate !src !prevState =
  let n = canvasSize prevState
      constantproperties = constants prevState
      dt = physConstantDt constantproperties
      diff = physConstantDiff constantproperties
      visc = physConstantVisc constantproperties
      zeroGrid = zeroMatrix (matrixDims $ waterDensity prevState)
      vstep = velStep n (velocityField prevState) (zeroGrid, zeroGrid) visc dt
      !(vField, dField) = updateWater (waterDensity prevState) src (heightMap prevState) vstep (mask prevState) dt n
      pigDens = 
        if isPigmentUpdate then snd $ updateWater (pigmentDensity prevState) src (heightMap prevState) vstep (mask prevState) dt n
        else pigmentDensity prevState
      (newPigmentLayer, newSurfaceLayer) =
        calculateSurfaceLayer pigDens (surfaceLayerDensity prevState) dField (heightMap prevState) dt
      (newCapillary, newShallowFluid, newMask) =
        simulateCapillaryFlow (capillaryLayerDensity prevState) dField (heightMap prevState) n diff dt
  in prevState { waterDensity = newShallowFluid
      , velocityField = vField
      , pigmentDensity = newPigmentLayer
      , surfaceLayerDensity = newSurfaceLayer
      , capillaryLayerDensity = newCapillary
      , heightMap = heightMap prevState
      , mask = newMask
      , constants = constantproperties
      }