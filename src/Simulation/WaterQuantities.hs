-- in reference to https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=dcf15363bc964044a554fc5f4af8e32101c083fe
module Simulation.WaterQuantities
  ( updateWater
  ) where

import           Data.Ord     (clamp)
import qualified Utils.Matrix as M
import           Utils.Matrix
import           Simulation.VelocityField (diffuse, advect)


type WaterQuantities = Matrix Double -- the density field corresponds to water here.

type VelocityFieldX = Matrix Double

type VelocityFieldY = Matrix Double

type Source = Matrix Double

addSource :: WaterQuantities -> Source -> Double -> WaterQuantities
addSource waterQuantities source dt =
  matrixImap (\idxs dprev -> dprev + dt * matrixGet idxs source) waterQuantities

-- 4.1 water diffusion
heightEqualizingVelocityField ::
     WaterQuantities -> Double -> Int -> (VelocityFieldX, VelocityFieldY) -- a linear relationship between water heights has been assumed
heightEqualizingVelocityField w coef n =
  let vx =
        M.matrixImapCheckbounds
          (1, 1, n, n)
          (\(x, y) _ ->
             coef * (M.matrixGet (x - 1, y) w - M.matrixGet (x + 1, y) w))
          w
      vy =
        M.matrixImapCheckbounds
          (1, 1, n, n)
          (\(x, y) _ ->
             coef * (M.matrixGet (x, y - 1) w - M.matrixGet (x, y + 1) w))
          w
   in (vx, vy)

addVfieldHeightDifferences ::
     (VelocityFieldX, VelocityFieldY)
  -> WaterQuantities
  -> Double
  -> Int
  -> (VelocityFieldX, VelocityFieldY)
addVfieldHeightDifferences v@(vx, vy) w dt n =
  let wh = 0.06
      wi = 1.0 - wh
      (vhx, vhy) = heightEqualizingVelocityField w 0.5 n
      vxnew =
        matrixImap (\idxs vprev -> vprev * wi + wh * matrixGet idxs vhx) vx
      vynew =
        matrixImap (\idxs vprev -> vprev * wi + wh * matrixGet idxs vhy) vy
   in (vxnew, vynew)

calculateVolDisplaced :: Double -> Double -> Double -> (Double, Double, Double -> Double -> Bool) -> Double
calculateVolDisplaced dt wPrev vCentre (vNeighbour, wNeighbour, fUpRight) =
    let vAvrg = vCentre + vNeighbour / 2 in
        if vAvrg `fUpRight` 0 then vAvrg * dt * wNeighbour
        else vAvrg * dt * wPrev

-- 4.1 water advection
advectWater ::
     (VelocityFieldX, VelocityFieldY)
  -> WaterQuantities
  -> Double
  -> Int
  -> WaterQuantities
advectWater v@(vx, vy) w dt n =
  matrixImapCheckbounds
    (1, 1, n, n)
    (\idxs@(i, j) wPrev ->
        let vCentrex = matrixGet idxs vx
            vCentrey = matrixGet idxs vy
            displacedVolumes = map (uncurry $ calculateVolDisplaced dt wPrev) [
                (vCentrex, (matrixGet (i + 1, j) vx, matrixGet (i + 1, j) w, (<))),
                (vCentrey, (matrixGet (i, j - 1) vy, matrixGet (i, j - 1) w, (<))),
                (vCentrex, (matrixGet (i - 1, j) vx, matrixGet (i - 1, j) w, (>))),
                (vCentrey, (matrixGet (i, j + 1) vy, matrixGet (i, j + 1) w, (>)))]
        in clamp (0, 100) (wPrev + 0.0001))
    w

-- need to do sides as well
evaporateWater :: WaterQuantities -> Double -> WaterQuantities
evaporateWater w dt =
  let eps = 0.0003
   in matrixMap (\wPrev -> clamp (0, 100) (wPrev - eps * dt)) w

diff :: Double
diff = 0.0001

-- exported methods
updateWater ::
     WaterQuantities
  -> WaterQuantities
  -> (VelocityFieldX, VelocityFieldY)
  -> Double
  -> Int
  -> ((VelocityFieldX, VelocityFieldY), WaterQuantities)
updateWater waterQuantities source v@(vx, vy) dt n =
  let vNew@(vxNew, vyNew) = addVfieldHeightDifferences v waterQuantities dt n
      wNew = addSource waterQuantities source dt
      diffused = diffuse n 0 source wNew diff dt
      advected = advectWater v diffused dt n
    --   evaporated = evaporateWater advected dt
   in (vNew, advected)
