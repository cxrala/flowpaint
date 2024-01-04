{-# LANGUAGE OverloadedStrings #-}

module Shallow
  ( Shallow,
  )
where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector2 as V2
import FRP.Yampa

-- for now, implemented almost identially to
-- http://graphics.cs.cmu.edu/nsp/course/15-464/Fall09/papers/StamFluidforGames.pdf

-- matrix --
-- to change to single vector later? adapted from yampa-game-of-life
type Matrix a = V.Vector (V.Vector a)

matrixMap :: (a -> b) -> Matrix a -> Matrix b
matrixMap f = V.map (V.map f)

matrixInitialise :: a -> Matrix a
matrixInitialise (x, y) val = replicate x (replicate y val)

matrixGet :: Matrix a -> (Int, Int) -> a
matrixGet m (x, y) = (m ! x) ! y

--- canvas density ---
data Canvas = Canvas
  { densityField :: Matrix Double,
    velocityField :: Matrix (Double, Double), -- x, y
    canvasSize :: (Int, Int) -- height, width
  }

type Source = Matrix Double

defaultCanvas :: (Int, Int) -> Canvas
defaultCanvas dimensions@(x, y) =
  Canvas
    { densityField = matrixInitialise dimensions 0,
      velocityField = matrixInitialise dimensions (0, 0),
      canvasSize = dimensions
    }

addSource :: Canvas -> Source -> Double -> Canvas
addSource canvas source dt =
  let newField =
        V.imap
          ( \idx row ->
              V.imap
                (\idy n -> n + dt * matrixGet source (idx, idy))
                row
          )
          $ densityField canvas
   in canvas
        { densityField = newField
        }

neighbours :: (Int, Int) -> Matrix a -> [a]
neighbours (i, j) m = map (`matrixGet` m) [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]

-- uses jacobi relaxation instead of gauss-seidel
-- note: should probably change to gauss-seidel if you can figure out how to make it pure.
-- why? convergence is faster (twice as fast) and also requires less storage.
diffuse :: Canvas -> Int -> Double -> Double -> Canvas
diffuse canvas b diff dt =
  let (height, width) = canvasSize canvas
   in let a = dt * diff * height * width
       in let x0 = densityField canvas
           in let x = matrixInitialise (canvasSize canvas) 0
               in let sumNeighbours = fold (+) neighbours
                   in canvas {densityField = iterate dif x !! 30}
  where
    dif =
      V.imap
        ( \idx row ->
            V.imap
              ( \idy _ -> (matrixGet (idx, idy) x0 + a * sumNeighbours (idx, idy) x) / (1 + 4 * a)
              )
              row
        )

advect :: Canvas -> Int -> Double -> Canvas
advect canvas b dt =
  let dField = densityField canvas
      vField = velocityField canvas
      (n, m) = canvasSize canvas
      dt0 = dt * n
      advected =
        V.imap
          ( \idx row ->
              V.imap
                ( \idy _ ->
                    let (vx, vy) = matrixGet (idx, idy) vField
                        lb = 0.5
                        ubx = n + 0.5
                        uby = m + 0.5
                        x =
                          let estimatedx = idx - dt0 * vx
                           in if estimatedx < lb
                                then lb
                                else
                                  if estimatedx > n + ubx
                                    then ubx
                                    else estimatedx
                        y =
                          let estimatedy = idy - dt0 * vy
                           in if estimatedy < lb
                                then lb
                                else
                                  if estimatedy > m + ubx
                                    then ubx
                                    else estimatedy
                     in let i0 = floor x
                            i1 = i0 + 1
                            j0 = floor y
                            j1 = j0 + 1
                            s1 = x - i0
                            s0 = 1 - s1
                            t1 = y - j0
                            t0 = 1 - t1
                         in s0 * (t0 * matrixGet (i0, j0) dField + t1 * matrixGet (i0, j1) dField)
                              + s1 * (t0 * matrixGet (i1, j0) dField + t1 * matrixGet (i1, j1) dField)
                )
                row
          )
          dField
   in canvas {densityField = advected}