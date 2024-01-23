-- in reference to https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=dcf15363bc964044a554fc5f4af8e32101c083fe
module Simulation.WaterQuantities (
    updateWater
) where 

import Utils.Matrix

type WaterQuantities = Matrix Double -- the density field corresponds to water here.

type VelocityFieldX = Matrix Double
type VelocityFieldY = Matrix Double

type Source = Matrix Double

addSource :: WaterQuantities -> Source -> Double -> WaterQuantities
addSource WaterQuantities source dt =
  matrixImap (\idxs dprev -> dprev + dt * matrixGet idxs source) WaterQuantities

heightEqualizingVelocityField :: WaterQuantities -> Double -> Int -> (VelocityFieldX, VelocityFieldY) -- a linear relationship between water heights has been assumed
heightEqualizingVelocityField w coef n =
    matrixImapCheckbounds (1, 1, n, n) (\idxs prev ->
        matrixNeighbours idxs w
        )

-- addVfieldHeightDifferences :: (VelocityFieldX, VelocityFieldY) -> Double -> Double -> (VelocityFieldX, VelocityFieldY)
-- addVfieldHeightDifferences v@(vx, vy) dt wh =
--     let wi = 1.0 - wh in
--     matrixImap (\idxs dprev -> dprev * wi + wh * ) v

-- exported methods

updateWater :: WaterQuantities -> WaterQuantities -> (VelocityFieldX, VelocityFieldY) -> Double -> ((VelocityFieldX, VelocityFieldY), WaterQuantities)
updateWater WaterQuantities source v@(vx, vy) dt =
    let vNew@(vxNew, vyNew) = addVfieldHeightDifferences v dt
    in (vNew, WaterQuantities)