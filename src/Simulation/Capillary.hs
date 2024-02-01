module Simulation.Capillary (
    genPoints,
    getWorleyNoise
) where

import Data.Vector (Vector, generate)
import Data.Hashable
import Utils.Matrix
import System.Random
import System.Random.Stateful
import Data.List (unfoldr, sort)
import qualified Data.Bifunctor

numP = 50
n = 5
clampPoints = 9

tablulateProbabilities :: Int -> Double -> Vector Double
tablulateProbabilities m lam =
    generate clampPoints (\x -> (lam^^(-m) * exp lam * fromIntegral (product [1 .. m]))^^(-1))

getNoise :: Int -> (Int, Int) -> Matrix Double
getNoise seed dims@(x, y) =
    let noise n = take n . unfoldr (Just . uniformR (0, 1))
        pureGen = mkStdGen seed
    in
        matrixFromList (noise (x * y) pureGen) dims

genPoints :: Int -> (Int, Int) -> [(Double, Double)]
genPoints seed dims@(x, y) =
    let noise n ub = take n . unfoldr (Just . uniformR (0, ub - 1))
        numPoints = numP
    in
        zip (noise numPoints (fromIntegral x) (mkStdGen seed)) (noise numPoints (fromIntegral  y) (mkStdGen (seed + 1)))

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x0, y0) (x1, y1) =
    sqrt ((x1 - x0) ** 2 + (y1 - y0) ** 2)

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