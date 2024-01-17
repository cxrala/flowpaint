module Simulation.VelocityField (
  densStep,
  velStep
)
where

import           Control.Exception (assert)
import           Data.Ord
import           Data.Vector       ((!))
import qualified Data.Vector       as V
import           Utils.Matrix

-- for now, implemented almost identically to
-- http://graphics.cs.cmu.edu/nsp/course/15-464/Fall09/papers/StamFluidforGames.pdf
type DensityField = Matrix Double

type VelocityFieldX = Matrix Double

type VelocityFieldY = Matrix Double

type Source = Matrix Double

addSource :: DensityField -> Source -> Double -> DensityField
addSource densityField source dt =
  matrixImap (\idxs dprev -> dprev + dt * matrixGet idxs source) densityField

-- uses jacobi relaxation instead of gauss-seidel
-- note: should probably change to gauss-seidel if you can figure out how to make it pure.
-- why? convergence is faster (twice as fast) and also requires less storage.
diffuse ::
     Int
  -> Int
  -> DensityField
  -> DensityField
  -> Double
  -> Double
  -> DensityField
diffuse n b x x0 diff dt =
  let a = dt * diff * fromIntegral n * fromIntegral n
      diffused =
        matrixSetBnd n b
          . matrixImapCheckbounds
              (1, 1, n, n)
              (\idxs dprev ->
                 (matrixGet idxs x0 + a * sum (matrixNeighbours idxs x))
                   / (1 + 4 * a))
   in (iterate diffused x !! 30)

advect ::
     Int
  -> Int
  -> DensityField
  -> VelocityFieldX
  -> VelocityFieldY
  -> Double
  -> DensityField
advect n b d0 u v dt =
  let dt0 = dt * fromIntegral n
      d =
        matrixImapCheckbounds
          (1, 1, n, n)
          (\idxs@(idx, idy) val ->
             let lb = 0.5
                 ub = fromIntegral n + 0.5
                 x = clamp (lb, ub) $ fromIntegral idx - dt0 * matrixGet idxs u
                 y = clamp (lb, ub) $ fromIntegral idy - dt0 * matrixGet idxs v
                 i0 = floor x
                 i1 = i0 + 1
                 j0 = floor y
                 j1 = j0 + 1
                 s1 = x - fromIntegral i0
                 s0 = 1 - s1
                 t1 = y - fromIntegral j0
                 t0 = 1 - t1
              in s0 * (t0 * matrixGet (i0, j0) d0 + t1 * matrixGet (i0, j1) d0)
                   + s1
                       * (t0 * matrixGet (i1, j0) d0
                            + t1 * matrixGet (i1, j1) d0))
          d0
   in matrixSetBnd n b d

project ::
     Int -> VelocityFieldX -> VelocityFieldY -> (VelocityFieldX, VelocityFieldY)
project n u v =
  let h = 1 / fromIntegral n
      div =
        matrixSetBnd n 0
          $ matrixImapCheckbounds
              (1, 1, n, n)
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
        let solvStep -- TODO: lift linsolver out of functions diffuse and here
             =
              matrixSetBnd n 0
                . matrixImapCheckbounds
                    (1, 1, n, n)
                    (\idxs dprev ->
                       (matrixGet idxs div + sum (matrixNeighbours idxs p)) / 4)
         in iterate solvStep p !! 30
      newU =
        matrixSetBnd n 1
          $ matrixImapCheckbounds
              (1, 1, n, n)
              (\(i, j) uprev ->
                 uprev
                   - 0.5
                       * (matrixGet (i + 1, j) psolv
                            - matrixGet (i - 1, j) psolv)
                       / h)
              u
      newV =
        matrixSetBnd n 2
          $ matrixImapCheckbounds
              (1, 1, n, n)
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
densStep ::
     Int
  -> DensityField
  -> DensityField
  -> VelocityFieldX
  -> VelocityFieldY
  -> Double
  -> Double
  -> DensityField
densStep n x x0 u v diff dt =
  let padN = n + 2
   in assert
        ((padN, padN) == matrixDims x
           && (padN, padN) == matrixDims x0
           && (padN, padN) == matrixDims u
           && (padN, padN) == matrixDims u)
        x0
        -- (let densAfterSource = addSource x x0 dt
        --      diffused = diffuse n 0 x0 densAfterSource diff dt
        --   in advect n 0 diffused u v dt)

-- x0 is initially the source vector
-- x is the original density vector
velStep ::
     Int
  -> VelocityFieldX
  -> VelocityFieldY
  -> VelocityFieldX
  -> VelocityFieldY
  -> Double
  -> Double
  -> (VelocityFieldX, VelocityFieldY)
velStep n u v u0 v0 visc dt =
  let padN = n + 2
    in assert
          ((padN, padN) == matrixDims u
              && (padN, padN) == matrixDims v
              && (padN, padN) == matrixDims u0
              && (padN, padN) == matrixDims v0)
          (let usrc = addSource u u0 dt
               vsrc = addSource v v0 dt
               diffusedu = diffuse n 1 u0 usrc visc dt
               diffusedv = diffuse n 2 v0 vsrc visc dt
               (uproj, vproj) = project n diffusedu diffusedv
               advu = advect n 1 usrc diffusedu diffusedv dt
               advv = advect n 1 vsrc diffusedu diffusedv dt
            in project n advu advv)
