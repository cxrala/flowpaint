module Simulation.SurfaceLayer (
    calculateSurfaceLayer
)
where

import Utils.Matrix
import Utils.Fields

maxWater = 1
pigdensity = 0.9
piggranulation = 0.5
pigstaining = 0.5

calculateNewSurfaceLayer :: ScalarField -> ScalarField -> ScalarField -> ScalarField -> Double -> ScalarField
calculateNewSurfaceLayer shallowPigmentField prevPigmentDeposited waterField heightMap dt =
    matrixImap (\idxs dprev -> dprev + dt * matrixGet idxs shallowPigmentField * (1 - (matrixGet idxs waterField / maxWater)) * (1 - matrixGet idxs heightMap * piggranulation)*pigdensity) prevPigmentDeposited

calculateNewShallowPigmentLayer :: ScalarField -> ScalarField -> ScalarField -> ScalarField -> Double -> ScalarField
calculateNewShallowPigmentLayer shallowPigmentField prevPigmentDeposited waterField heightMap dt =
    matrixImap (\idxs dprev -> dprev + dt * matrixGet idxs prevPigmentDeposited * (matrixGet idxs waterField / maxWater) * (1 - (1 - matrixGet idxs heightMap) * piggranulation)*(pigdensity/pigstaining)) shallowPigmentField

-- shallowPigmentLayer, surfaceLayer
calculateSurfaceLayer :: ScalarField -> ScalarField -> ScalarField -> ScalarField -> Double -> (ScalarField, ScalarField)
calculateSurfaceLayer shallowPigmentField prevPigmentDeposited waterField heightMap dt =
    (
        calculateNewShallowPigmentLayer shallowPigmentField prevPigmentDeposited waterField heightMap dt,
        calculateNewSurfaceLayer shallowPigmentField prevPigmentDeposited waterField heightMap dt
    )