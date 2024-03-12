module Simulation.SurfaceLayer
  ( calculateSurfaceLayer,
  normaliseSurfaceLayers
  ) where

import           Utils.Fields
import           Utils.Matrix
import Debug.Trace (trace)
import Data.Ord (clamp)

maxWater = 1

pigdensity = 0.9

piggranulation = 0.5

pigstaining = 0.5

calculateNewSurfaceLayer ::
     ScalarField
  -> ScalarField
  -> ScalarField
  -> ScalarField
  -> Double
  -> ScalarField
calculateNewSurfaceLayer shallowPigmentField prevPigmentDeposited waterField heightMap dt =
  matrixImap
    (\idxs dprev ->
       dprev
         + dt
             * matrixGet idxs shallowPigmentField
             * (1 - (matrixGet idxs waterField / maxWater))
             * (1 - matrixGet idxs heightMap * piggranulation)
             * pigdensity)
    prevPigmentDeposited

calculateNewShallowPigmentLayer ::
     ScalarField
  -> ScalarField
  -> ScalarField
  -> ScalarField
  -> Double
  -> ScalarField
calculateNewShallowPigmentLayer shallowPigmentField prevPigmentDeposited waterField heightMap dt =
  matrixImap
    (\idxs dprev ->
       dprev
         + dt
             * matrixGet idxs prevPigmentDeposited
             * (matrixGet idxs waterField / maxWater)
             * (1 - (1 - matrixGet idxs heightMap) * piggranulation)
             * (pigdensity / pigstaining))
    shallowPigmentField

-- shallowPigmentLayer, surfaceLayer
calculateSurfaceLayer ::
     ScalarField
  -> ScalarField
  -> ScalarField
  -> ScalarField
  -> Double
  -> (ScalarField, ScalarField)
calculateSurfaceLayer shallowPigmentField prevPigmentDeposited waterField heightMap dt =
    (calculateNewShallowPigmentLayer
        shallowPigmentField
        prevPigmentDeposited
        waterField
        heightMap
        dt
    , matrixMap (clamp (0, 10)) $ calculateNewSurfaceLayer
        shallowPigmentField
        prevPigmentDeposited
        waterField
        heightMap
        dt)

-- normalises the surface layers (r, y, b) to between 0 - 5 based on quantity of applied pigment, such that their sum = 5 (arbitrarily)
normaliseSurfaceLayers ::
     (ScalarField, ScalarField, ScalarField) -> (ScalarField, ScalarField, ScalarField)
normaliseSurfaceLayers (rlayer, ylayer, blayer) =
  let tmpMat = matrixImap
        (\i rval ->
          let pigr = rval
              pigy = matrixGet i ylayer
              pigb = matrixGet i blayer
              sum = (pigr + pigy + pigb) / 5
            in
              if pigr + pigy + pigb <= 5 then (pigr, pigy, pigb) else
              (pigr / sum, pigy / sum, pigb / sum)) -- trace (trace (show (sum, pigr, pigy, pigb)) undefined) 
        rlayer
    in expandMatrix tmpMat

-- TODO: slow...
expandMatrix :: Matrix (Double, Double, Double) -> (ScalarField, ScalarField, ScalarField)
expandMatrix m =
  (matrixMap (\(r, _, _) -> r) m, matrixMap (\(_, y, _) -> y) m, matrixMap (\(_, _, b) -> b) m)
