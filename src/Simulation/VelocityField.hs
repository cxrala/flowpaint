{-# LANGUAGE BangPatterns #-}
module Simulation.VelocityField
  ( velStep
  , diffuse
  , advect
  ) where

import           Control.Exception (assert)
import           Data.Ord
import           Data.Vector       ((!))
import qualified Data.Vector       as V
import           Utils.Fields
import           Utils.Matrix
import Data.List (iterate')

-- for now, implemented almost identically to
-- http://graphics.cs.cmu.edu/nsp/course/15-464/Fall09/papers/StamFluidforGames.pdf
type VelocityFieldComponent = Matrix Double

addSource :: ScalarField -> ScalarField -> Double -> ScalarField
addSource densityField source dt =
  matrixImap (\idxs dprev -> dprev + dt * matrixGet idxs source) densityField

-- uses jacobi relaxation instead of gauss-seidel
-- note: should probably change to gauss-seidel if you can figure out how to make it pure.
-- why? convergence is faster (twice as fast) and also requires less storage.
diffuse ::
     Int -> Int -> ScalarField -> ScalarField -> Double -> Double -> ScalarField
diffuse n b x x0 diff dt =
  let a = dt * diff * fromIntegral n * fromIntegral n
      !diffused =
        matrixSetBnd n b
          . (\y ->
               matrixImapCheckbounds
                 ((1, 1), (n, n))
                 (\idxs dprev ->
                    (matrixGet idxs x0 + a * sum (matrixNeighbours idxs y))
                      / (1 + 4 * a))
                 y)
   in iterate diffused x !! 5

advect :: Int -> Int -> ScalarField -> VelocityField -> Double -> ScalarField
advect !n !b !d0 (u, v) dt =
  let !dt0 = dt * fromIntegral n
      !d =
        matrixImapCheckbounds
          ((1, 1), (n, n))
          (\idxs@(idx, idy) val ->
             val)
          d0
   in matrixSetBnd n b d

project ::
     Int -> VelocityFieldComponent -> VelocityFieldComponent -> VelocityField
project n u v =
  let h = 1 / fromIntegral n
      !div =
        matrixSetBnd n 0
          $ matrixImapCheckbounds
              ((1, 1), (n, n))
              (\(i, j) a ->
                 -0.5
                   * h
                   * (matrixGet (i + 1, j) u
                        - matrixGet (i - 1, j) u
                        + matrixGet (i, j + 1) v
                        - matrixGet (i, j - 1) v))
              u
      p = matrixInit (matrixDims u) 0
      psolv =
        let !solvStep -- TODO: lift linsolver out of functions diffuse and here
             =
              matrixSetBnd n 0
                . (\y ->
                     matrixImapCheckbounds
                       ((1, 1), (n, n))
                       (\idxs dprev ->
                          (matrixGet idxs div + sum (matrixNeighbours idxs y))
                            / 4)
                       y)
         in iterate solvStep p !! 5
      !newU =
        matrixSetBnd n 1
          $ matrixImapCheckbounds
              ((1, 1), (n, n))
              (\(i, j) uprev ->
                 uprev
                   - 0.5
                       * (matrixGet (i + 1, j) psolv
                            - matrixGet (i - 1, j) psolv)
                       / h)
              u
      !newV =
        matrixSetBnd n 2
          $ matrixImapCheckbounds
              ((1, 1), (n, n))
              (\(i, j) vprev ->
                 vprev
                   - 0.5
                       * (matrixGet (i, j + 1) psolv
                            - matrixGet (i, j - 1) psolv)
                       / h)
              v
   in (newU, newV)

-- x0 is initially the source vector
-- x is the original density vector
-- densStep ::
--      Int
--   -> ScalarField
--   -> ScalarField
--   -> VelocityField
--   -> Double
--   -> Double
--   -> ScalarField
-- densStep n dPrev source (u, v) diff dt =
--   let padN = n + 2
--    in assert
--         ((padN, padN) == matrixDims dPrev
--            && (padN, padN) == matrixDims source
--            && (padN, padN) == matrixDims u
--            && (padN, padN) == matrixDims v)
--         (let densAfterSource = addSource dPrev source dt
--              diffused = diffuse n 0 source densAfterSource diff dt
--              in advect n 0 diffused (u, v) dt)
-- x0 is initially the source vector
-- x is the original density vector
velStep ::
     Int -> VelocityField -> VelocityField -> Double -> Double -> VelocityField
velStep n (u, v) (u0, v0) visc dt =
  let padN = n + 2
   in assert
        ((padN, padN) == matrixDims u
           && (padN, padN) == matrixDims v
           && (padN, padN) == matrixDims u0
           && (padN, padN) == matrixDims v0)
        (let !usrc = addSource u u0 dt
             !vsrc = addSource v v0 dt
             !diffusedu = diffuse n 1 u0 usrc visc dt
             !diffusedv = diffuse n 2 v0 vsrc visc dt
             !(uproj, vproj) = project n diffusedu diffusedv
             !advu = advect n 1 usrc (diffusedu, diffusedv) dt
             !advv = advect n 1 vsrc (diffusedu, diffusedv) dt
          in project n advu advv)
