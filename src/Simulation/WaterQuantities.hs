-- in reference to https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=dcf15363bc964044a554fc5f4af8e32101c083fe
module Simulation.WaterQuantities
  ( updateWater
  ) where

import           Data.Ord                 (clamp)
import           Simulation.VelocityField (advect, diffuse)
import           Utils.Fields             (ScalarField, VelocityField,
                                           elementwiseCombine)
import           Utils.Matrix             (Matrix, matrixGet,
                                           matrixImapCheckbounds, matrixMap, matrixInit, matrixDims)

-- some global constants. TODO: figure out exactly how to structure these.
wh = 0.06

wi = 1 - wh

epsEvaporation = 0.003
diff = 0.0001

-- This updated velocity field models the tendency for water to "fill unused spaces" -- leading to the dark edge effect. See 4.1, water diffusion.
addVfieldHeightDifferences ::
     VelocityField -> ScalarField -> Double -> Int -> VelocityField
addVfieldHeightDifferences v@(vx, vy) w dt n =
  let (vhx, vhy) = heightEqualizingVelocityField w 0.05 n
      getNewVelocities =
        elementwiseCombine (\vField hField -> vField * wi + hField * wh)
   in (getNewVelocities vx vhx, getNewVelocities vy vhy)
  where
    heightEqualizingVelocityField waterField coef n =
      let adjacent isX (x, y)
            | isX = ((x - 1, y), (x + 1, y))
            | otherwise = ((x, y - 1), (x, y + 1))
          updateVelocityField isX =
            matrixImapCheckbounds
              (1, 1, n, n)
              (\(x, y) _ ->
                 let (n1, n2) = adjacent isX (x, y)
                  in coef * (matrixGet n1 waterField - matrixGet n2 waterField))
              waterField
       in (updateVelocityField True, updateVelocityField False)

setNormalToZero :: VelocityField -> Matrix Bool -> VelocityField
setNormalToZero (vx, vy) mask =
  let f mask val =
        if mask
          then val
          else 0
   in (elementwiseCombine f mask vx, elementwiseCombine f mask vy)

-- Some epsilon amount of water evaporates every timestep. Only the top is implemented. TODO: implement side evaporation.
evaporateWater :: ScalarField -> Double -> ScalarField
evaporateWater w dt =
  matrixMap (\wPrev -> max 0 (wPrev - epsEvaporation * dt)) w

-- Source addition.
addSource ::
     ScalarField -> ScalarField  -> Matrix Bool -> Double -> (ScalarField, Matrix Bool)
addSource waterQuantities source mask dt =
  ( elementwiseCombine (\a b -> a + dt * b) waterQuantities source
  , elementwiseCombine (\src mPrev -> src > 0 || mPrev) source mask)

-- 4.1 Diffusion. TODO: for now, equivalent to velocity field implementation.
diffuseWater ::
     Int -> Int -> ScalarField -> ScalarField -> Double -> Double -> ScalarField
diffuseWater = diffuse

-- 4.1 Advection. TODO: for now, equivalent to velocity field implementation.
advectWater ::
     Int -> Int -> ScalarField -> VelocityField -> Double -> ScalarField
advectWater = advect

-- calculateVolDisplaced :: Double -> Double -> Double -> (Double, Double, Double -> Double -> Bool) -> Double
-- calculateVolDisplaced dt wPrev vCentre (vNeighbour, wNeighbour, fUpRight) =
--     let vAvrg = vCentre + vNeighbour / 2 in
--         if vAvrg `fUpRight` 0 then vAvrg * dt * wNeighbour
--         else vAvrg * dt * wPrev
-- -- 4.1 water advection
-- advectWater ::
--      VelocityField
--   -> ScalarField
--   -> Double
--   -> Int
--   -> ScalarField
-- advectWater v@(vx, vy) w dt n =
--   matrixImapCheckbounds
--     (1, 1, n, n)
--     (\idxs@(i, j) wPrev ->
--         let vCentrex = matrixGet idxs vx
--             vCentrey = matrixGet idxs vy
--             displacedVolumes = map (uncurry $ calculateVolDisplaced dt wPrev) [
--                 (vCentrex, (matrixGet (i + 1, j) vx, matrixGet (i + 1, j) w, (<))),
--                 (vCentrey, (matrixGet (i, j - 1) vy, matrixGet (i, j - 1) w, (<))),
--                 (vCentrex, (matrixGet (i - 1, j) vx, matrixGet (i - 1, j) w, (>))),
--                 (vCentrey, (matrixGet (i, j + 1) vy, matrixGet (i, j + 1) w, (>)))]
--         in clamp (0, 100) (wPrev + 0.0001))
--     w
-- EXPORTED
updateWater ::
     ScalarField
  -> ScalarField
  -> ScalarField
  -> VelocityField
  -> Matrix Bool
  -> Double
  -> Int
  -> (VelocityField, ScalarField)
updateWater waterQuantities source heightMap v@(vx, vy) mask dt n =
  let vNew = addVfieldHeightDifferences v waterQuantities dt n
      vBoundaries@(vxNew, vyNew) = setNormalToZero vNew mask -- Boundary conditions
      (wNew, maskNew) = addSource waterQuantities source mask dt
      diffused = diffuseWater n 0 source wNew diff dt
      advected = advectWater n 0 diffused (vxNew, vyNew) dt
      clamped =
        elementwiseCombine
          (\height water -> clamp (0, height) water)
          heightMap
          advected
      masked =
        elementwiseCombine
          (\mval val ->
             if mval
               then val
               else 0)
          maskNew
          clamped
      -- clamped = matrixMap (clamp (0, 1)) advected
      evaporated = evaporateWater masked dt
   in (vNew, evaporated)
