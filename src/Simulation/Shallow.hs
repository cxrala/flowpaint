module Shallow where

import           Data.Vector         ((!))
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV

-- for now, implemented almost identically to
-- http://graphics.cs.cmu.edu/nsp/course/15-464/Fall09/papers/StamFluidforGames.pdf
-- matrix --
-- to change to single vector later? adapted from yampa-game-of-life
type Matrix a = V.Vector (V.Vector a)

matrixMap :: (a -> b) -> Matrix a -> Matrix b
matrixMap f = V.map (V.map f)

matrixInitialise :: (Int, Int) -> a -> Matrix a
matrixInitialise (rows, cols) val = V.replicate rows (V.replicate cols val)

matrixGet :: (Int, Int) -> Matrix a -> a
matrixGet (x, y) m = (m ! x) ! y

--- canvas density ---
data Canvas = Canvas
  { densityField  :: Matrix Double
  , velocityField :: Matrix (Double, Double) -- x, y
  , canvasSize    :: (Int, Int) -- height, width
  }

type Source = Matrix Double

defaultCanvas :: (Int, Int) -> Canvas
defaultCanvas dimensions@(x, y) =
  Canvas
    { densityField = matrixInitialise dimensions 0
    , velocityField = matrixInitialise dimensions (0, 0)
    , canvasSize = dimensions
    }

addSource :: Canvas -> Source -> Double -> Canvas
addSource canvas source dt =
  let newField =
        V.imap
          (\idx row ->
             V.imap (\idy n -> n + dt * matrixGet (idx, idy) source) row)
          $ densityField canvas
   in canvas {densityField = newField}

neighbours :: (Int, Int) -> Matrix a -> [a]
neighbours (i, j) m =
  map (\(x, y) -> matrixGet (x, y) m) [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]

-- uses jacobi relaxation instead of gauss-seidel
-- note: should probably change to gauss-seidel if you can figure out how to make it pure.
-- why? convergence is faster (twice as fast) and also requires less storage.
diffuse :: Canvas -> Int -> Double -> Double -> Canvas
diffuse canvas b diff dt =
  let (height, width) = fromIntegralPair $ canvasSize canvas
      a = dt * diff * height * width
      x0 = densityField canvas
      x = matrixInitialise (canvasSize canvas) 0
      dif =
        V.imap
          (\idx row ->
              V.imap
                (\idy _ ->
                  (matrixGet (idx, idy) x0 + a * sum (neighbours (idx, idy) x))
                    / (1 + 4 * a))
                row)
      in canvas {densityField = iterate dif x !! 30}

advect :: Canvas -> Int -> Double -> Canvas
advect canvas b dt =
  let dField = densityField canvas
      vField = velocityField canvas
      (n, m) = fromIntegralPair $ canvasSize canvas
      dt0 = dt * n
      advected =
        V.imap
          (\idx row ->
             V.imap
               (\idy _ ->
                  let (vx, vy) = matrixGet (idx, idy) vField
                      lb = 0.5
                      ubx = n + 0.5
                      uby = m + 0.5
                      x =
                        let estimatedx = fromIntegral idx - dt0 * vx
                         in if estimatedx < lb
                              then lb
                              else if estimatedx > n + ubx
                                     then ubx
                                     else estimatedx
                      y =
                        let estimatedy = fromIntegral idy - dt0 * vy
                         in if estimatedy < lb
                              then lb
                              else if estimatedy > m + ubx
                                     then ubx
                                     else estimatedy
                   in let i0 = floor x
                          i1 = i0 + 1
                          j0 = floor y
                          j1 = j0 + 1
                          s1 = x - fromIntegral i0
                          s0 = 1 - s1
                          t1 = y - fromIntegral j0
                          t0 = 1 - t1
                       in s0 * (t0 * matrixGet (i0, j0) dField
                            + t1 * matrixGet (i0, j1) dField)
                          + s1 * (t0 * matrixGet (i1, j0) dField
                            + t1 * matrixGet (i1, j1) dField))
               row)
          dField
   in canvas {densityField = advected}


-- random util (move out later? or better way?) --
fromIntegralPair = (\f (a, b) -> (f a, f b)) fromIntegral