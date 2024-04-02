{-# LANGUAGE BangPatterns #-}
module Simulation.Capillary
  ( genPoints
  , getWorleyNoise
  , simulateCapillaryFlow
  ) where

import qualified Data.Bifunctor
import           Data.Hashable
import           Data.List              (sort, unfoldr)
import           Data.Vector            (Vector, generate)
import System.Random ( uniformR, mkStdGen )
import           System.Random.Stateful
import           Utils.Fields
import           Utils.Matrix
import Simulation.VelocityField (diffuse)
import Data.Ord (clamp)


-- TODO: refactor, this is a mess

numP = 2

n = 1
threshhold = 0.005
evaporation = 0.07
alpha = 3

genPoints :: Int -> (Int, Int) -> [(Double, Double)]
genPoints seed dims@(x, y) =
  let noise n ub = take n . unfoldr (Just . uniformR (0, ub - 1))
      numPoints = numP
   in zip
        (noise numPoints (fromIntegral x) (mkStdGen seed))
        (noise numPoints (fromIntegral y) (mkStdGen (seed + 1)))

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x0, y0) (x1, y1) = sqrt ((x1 - x0) ** 2 + (y1 - y0) ** 2)

remap :: (Double, Double) -> (Double, Double) -> Double -> Double
remap (a, b) (a1, b1) x = a1 + (b1 - a1) * (x - a) / (b - a)

helperWorley :: [(Double, Double)] -> Int -> (Int, Int) -> Double
helperWorley points width point@(x, y) =
  let sorted = sort $ map (dist (fromIntegral x, fromIntegral y)) points
   in remap (0, fromIntegral width) (0, 1) (sorted !! n)

getWorleyNoise :: [(Double, Double)] -> (Int, Int) -> Matrix Double
getWorleyNoise points dims@(x, _) = matrixGenerate dims (helperWorley points x)

-- getWorleyNoise :: [(Double, Double)] -> (Int, Int) -> Matrix Double
-- getWorleyNoise points dims@(x, _) =
--     let mapped_points = map (Data.Bifunctor.bimap floor floor) points in
--     matrixGenerate dims (\(i, j) -> if (i, j) `elem` mapped_points then 1 else 0)
-- https://dl.acm.org/doi/pdf/10.1145/258734.258896 4.5
absorbShallow ::
     ScalarField
  -> ScalarField
  -> ScalarField
  -> Double
  -> (ScalarField, ScalarField)
absorbShallow capillaryLayer shallowFluidLayer heightMap dt =
  let differences =
        matrixImap
          (\idxs cprev ->
             let availableCapacity = matrixGet idxs heightMap - cprev
              in minimum
                   [ availableCapacity
                   , alpha * dt
                   , matrixGet idxs shallowFluidLayer
                   ])
          capillaryLayer
   in ( elementwiseCombine (+) capillaryLayer differences
      , elementwiseCombine (\a b -> a - (b/alpha)) shallowFluidLayer differences)

diffuseCapillary :: ScalarField -> ScalarField -> Int -> Double -> Double -> ScalarField
diffuseCapillary capillaryLayer heightMap n = diffuse n 0 (matrixInit (matrixDims capillaryLayer) 0) capillaryLayer

evaporateCapillary :: ScalarField -> ScalarField -> Double -> ScalarField
evaporateCapillary capillaryLayer shallowFieldLayer dt =
  elementwiseCombine (\c s -> if s == 0 then max 0 (c - evaporation * dt) else c) capillaryLayer shallowFieldLayer

getMask :: ScalarField -> ScalarField -> Matrix Bool
getMask = elementwiseCombine (\quantity height -> quantity / height > threshhold)

simulateCapillaryFlow :: ScalarField -> ScalarField -> ScalarField -> Int -> Double -> Double -> (ScalarField, ScalarField, Matrix Bool)
simulateCapillaryFlow capillaryLayer shallowFluidLayer heightMap n diff dt =
  let (newCapillary, newShallowFluid) = absorbShallow capillaryLayer shallowFluidLayer heightMap dt
      !diffused = diffuseCapillary newCapillary heightMap n diff dt
      evaporated = evaporateCapillary diffused newShallowFluid dt in 
        (evaporated, newShallowFluid, getMask capillaryLayer heightMap)