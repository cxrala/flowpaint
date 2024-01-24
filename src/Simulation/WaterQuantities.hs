-- in reference to https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=dcf15363bc964044a554fc5f4af8e32101c083fe
module Simulation.WaterQuantities (
    updateWater,
    addVfieldHeightDifferences
) where 

import Utils.Matrix
import qualified Utils.Matrix as M

type WaterQuantities = Matrix Double -- the density field corresponds to water here.

type VelocityFieldX = Matrix Double
type VelocityFieldY = Matrix Double

type Source = Matrix Double

addSource :: WaterQuantities -> Source -> Double -> WaterQuantities
addSource waterQuantities source dt =
  matrixImap (\idxs dprev -> dprev + dt * matrixGet idxs source) waterQuantities

heightEqualizingVelocityField :: WaterQuantities -> Double -> Int -> (VelocityFieldX, VelocityFieldY) -- a linear relationship between water heights has been assumed
heightEqualizingVelocityField w coef n =
    let vx = M.matrixImapCheckbounds (1, 1, n, n) (\(x, y) _ -> coef * (M.matrixGet (x - 1, y) w  - M.matrixGet (x + 1, y) w)) w
        vy = M.matrixImapCheckbounds (1, 1, n, n) (\(x, y) _ -> coef * (M.matrixGet (x, y - 1) w  - M.matrixGet (x, y + 1) w)) w
    in (vx, vy)

addVfieldHeightDifferences :: (VelocityFieldX, VelocityFieldY) -> WaterQuantities -> Double -> Int -> (VelocityFieldX, VelocityFieldY)
addVfieldHeightDifferences v@(vx, vy) w dt n =
    let wh = 0.06
        wi = 1.0 - wh
        (vhx, vhy) = heightEqualizingVelocityField w 0.5 n
        vxnew = matrixImap (\idxs vprev -> vprev * wi + wh * matrixGet idxs vhx) vx
        vynew = matrixImap (\idxs vprev -> vprev * wi + wh * matrixGet idxs vhy) vy
    in (vxnew, vynew)

-- exported methods

updateWater :: WaterQuantities -> WaterQuantities -> (VelocityFieldX, VelocityFieldY) -> Double -> Int -> ((VelocityFieldX, VelocityFieldY), WaterQuantities)
updateWater waterQuantities source v@(vx, vy) dt n =
    let vNew@(vxNew, vyNew) = addVfieldHeightDifferences v waterQuantities dt n
    in (vNew, waterQuantities)