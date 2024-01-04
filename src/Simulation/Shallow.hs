module Shallow where

import           Data.Vector         ((!))
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV

-- for now, implemented almost identically to
-- http://graphics.cs.cmu.edu/nsp/course/15-464/Fall09/papers/StamFluidforGames.pdf
-- matrix --
-- to change to single vector later? adapted from yampa-game-of-life
-- type Matrix a = V.Vector (V.Vector a)
-- matrixInitialise :: (Int, Int) -> a -> Matrix a
-- matrixInitialise (rows, cols) val = V.replicate rows (V.replicate cols val)
-- matrixGet :: (Int, Int) -> Matrix a -> a
-- matrixGet (x, y) m = (m ! x) ! y
type Matrix a = V.Vector a

flattenDims :: (Int, Int) -> Int -> Int
flattenDims (x, y) width = x + width * y

expandDims :: Int -> Int -> (Int, Int)
expandDims i width = (i `mod` width, i `div` width)

matrixInitialise :: (Int, Int) -> a -> Matrix a
matrixInitialise (nrows, ncols) = V.replicate (nrows * ncols)

matrixGet :: (Int, Int) -> Matrix a -> Int -> a
matrixGet dims m width = m ! flattenDims dims width

--- canvas density ---
data Canvas = Canvas
  { densityField  :: Matrix Double
  , velocityField :: Matrix (Double, Double) -- x, y
  , canvasSize    :: (Int, Int) -- height, width
  }

type Source = Matrix Double

defaultCanvas :: (Int, Int) -> Canvas
defaultCanvas dimensionsNonPad@(x, y) =
  Canvas
    { densityField = matrixInitialise (x + 2, y + 2) 0
    , velocityField = matrixInitialise (x + 2, y + 2) (0, 0)
    , canvasSize = (x + 2, y + 2)
    }

withinBounds :: (Int, Int) -> Canvas -> Bool
withinBounds (x, y) canvas =
  x > 0
    && y > 0
    && x < fst (canvasSize canvas) - 1
    && y < snd (canvasSize canvas) - 1

addSource :: Canvas -> Source -> Double -> Canvas
addSource canvas source dt =
  let width = fst (canvasSize canvas)
      newField =
        V.imap
          (\i n ->
             let (idx, idy) = expandDims i width
              in n + dt * matrixGet (idx, idy) source width)
          $ densityField canvas
   in canvas {densityField = newField}

neighbours :: (Int, Int) -> Matrix a -> Canvas -> [a]
neighbours (i, j) m canvas =
  let width = fst (canvasSize canvas)
   in map
        (\(x, y) -> matrixGet (x, y) m width)
        [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]

-- uses jacobi relaxation instead of gauss-seidel
-- note: should probably change to gauss-seidel if you can figure out how to make it pure.
-- why? convergence is faster (twice as fast) and also requires less storage.
diffuse :: Canvas -> Int -> Double -> Double -> Canvas
diffuse canvas b diff dt =
  let width = fst (canvasSize canvas)
      (h, w) = fromIntegralPair $ canvasSize canvas
      a = dt * diff * h * w
      x0 = densityField canvas
      x = matrixInitialise (canvasSize canvas) 0
      dif =
        V.imap
          (\i n ->
             let (idx, idy) = expandDims i width
              in if withinBounds (idx, idy) canvas
                   then (matrixGet (idx, idy) x0 width
                           + a * sum (neighbours (idx, idy) x canvas))
                          / (1 + 4 * a)
                   else n)
   in canvas {densityField = iterate dif x !! 30}

advect :: Canvas -> Int -> Double -> Canvas
advect canvas b dt =
  let width = fst (canvasSize canvas)
      dField = densityField canvas
      vField = velocityField canvas
      (n, m) = fromIntegralPair $ canvasSize canvas
      dt0 = dt * n
      advected =
        V.imap
          (\i val ->
             let (idx, idy) = expandDims i width
              in if withinBounds (idx, idy) canvas
                   then let (vx, vy) = matrixGet (idx, idy) vField width
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
                             in s0
                                  * (t0 * matrixGet (i0, j0) dField width
                                       + t1 * matrixGet (i0, j1) dField width)
                                  + s1
                                      * (t0 * matrixGet (i1, j0) dField width
                                           + t1
                                               * matrixGet (i1, j1) dField width)
                   else val)
          dField
   in canvas {densityField = advected}

-- random util (move out later? or better way?) --
fromIntegralPair = (\f (a, b) -> (f a, f b)) fromIntegral
