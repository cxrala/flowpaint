module Utils.Matrix
  ( Matrix
  , matrixDims
  , matrixInit
  , matrixGet
  , matrixMap
  , matrixImap
  , matrixImapCheckbounds
  , matrixNeighbours
  ) where

import           Data.Tuple
import           Data.Vector ((!))
import qualified Data.Vector as V

data Matrix a = Matrix
  { vector :: V.Vector a
  , dims   :: (Int, Int) -- height, width
  }

-- private
mWidth :: Matrix a -> Int
mWidth = snd . dims

flattenDims :: (Int, Int) -> Matrix a -> Int
flattenDims (x, y) m = x + mWidth m * y

raiseDims :: Int -> Matrix a -> (Int, Int)
raiseDims i m = quotRem i (mWidth m)

-- exposed
matrixDims :: Matrix a -> (Int, Int)
matrixDims = dims

matrixInit :: (Int, Int) -> a -> Matrix a
matrixInit dims@(rows, cols) val =
  Matrix {vector = V.replicate rows val, dims = dims}

matrixGet :: (Int, Int) -> Matrix a -> a
matrixGet ij m = vector m ! flattenDims ij m

matrixMap :: (a -> b) -> Matrix a -> Matrix b
matrixMap f m = m {vector = V.map f (vector m)}

matrixImap :: ((Int, Int) -> a -> b) -> Matrix a -> Matrix b
matrixImap f m = m {vector = V.imap (\i -> f (raiseDims i m)) (vector m)}

-- INCLUSIVE within bounds check.
matrixImapCheckbounds ::
     (Int, Int, Int, Int) -> ((Int, Int) -> a -> a) -> Matrix a -> Matrix a
matrixImapCheckbounds bounds f m =
  m
    { vector =
              V.imap (\i a ->
              let point = raiseDims i m in
              if withinBounds point bounds
                then a
                else f point a) (vector m)
    }
  where
    withinBounds (x, y) (lbx, lby, ubx, uby) =
      x >= lbx && y >= lby && x <= ubx && x <= uby

matrixNeighbours :: (Int, Int) -> Matrix a -> [a]
matrixNeighbours (i, j) m = map (`matrixGet` m) [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]