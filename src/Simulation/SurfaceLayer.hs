module Simulation.SurfaceLayer (
    calculateSurfaceLayer
)
where

import Utils.Matrix

type DensityField = Matrix Double

maxWater = 1
h = 0.5
pigdensity = 0.5
piggranulation = 0.5
pigstaining = 0.5

calculateNewSurfaceLayer :: DensityField -> DensityField -> DensityField -> Double -> DensityField
calculateNewSurfaceLayer shallowPigmentField prevPigmentDeposited waterField dt =
    matrixImap (\idxs dprev -> dprev + dt * matrixGet idxs shallowPigmentField * (1 - (matrixGet idxs waterField / maxWater)) * (1 - h * piggranulation)*pigdensity) prevPigmentDeposited

calculateNewShallowPigmentLayer :: DensityField -> DensityField -> DensityField -> Double -> DensityField
calculateNewShallowPigmentLayer shallowPigmentField prevPigmentDeposited waterField dt =
    matrixImap (\idxs dprev -> dprev + dt * matrixGet idxs prevPigmentDeposited * (matrixGet idxs waterField / maxWater) * (1 - (1 - h) * piggranulation)*(pigdensity/pigstaining)) shallowPigmentField

-- shallowPigmentLayer, surfaceLayer
calculateSurfaceLayer :: DensityField -> DensityField -> DensityField -> Double -> (DensityField, DensityField)
calculateSurfaceLayer shallowPigmentField prevPigmentDeposited waterField dt =
    (
        calculateNewShallowPigmentLayer shallowPigmentField prevPigmentDeposited waterField dt,
        calculateNewSurfaceLayer shallowPigmentField prevPigmentDeposited waterField dt
    )