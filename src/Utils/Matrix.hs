{-# LANGUAGE RankNTypes #-}

module Utils.Matrix
  ( Matrix
  , matrixDims
  , matrixInit
  , matrixGet
  , matrixMap
  , matrixImap
  , matrixImapCheckbounds
  , matrixNeighbours
  , matrixUnsafeUpd
  , matrixSetBnd
  , matrixGenerate
  ) where

import           Control.Monad
import           Control.Monad.ST
import           Data.Tuple
import           Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

data Matrix a = Matrix
  { vector :: !(V.Vector a)
  , dims   :: !(Int, Int) -- height, width
  } deriving Show

-- private
mWidth :: Matrix a -> Int
mWidth = snd . dims

flattenDims :: (Int, Int) -> Matrix a -> Int
flattenDims (x, y) m = x + mWidth m * y

raiseDimsWidth :: Int -> Int -> (Int, Int)
raiseDimsWidth i j = swap $ quotRem i j

raiseDims :: Int -> Matrix a -> (Int, Int)
raiseDims i m = raiseDimsWidth i (mWidth m)

-- exposed
matrixDims :: Matrix a -> (Int, Int)
matrixDims = dims

matrixInit :: V.Unbox a => (Int, Int) -> a -> Matrix a
matrixInit dims@(rows, cols) val =
  Matrix {vector = V.replicate (rows * cols) val, dims = dims}

matrixGenerate :: V.Unbox a => (Int, Int) -> ((Int, Int) -> a) -> Matrix a
matrixGenerate dims@(x, y) f =
  Matrix {vector = V.generate (x * y) $ f . (`raiseDimsWidth` y), dims = dims}

matrixGet :: V.Unbox a => (Int, Int) -> Matrix a -> a
matrixGet ij m = vector m ! flattenDims ij m

matrixMap :: (V.Unbox a, V.Unbox b) => (a -> b) -> Matrix a -> Matrix b
matrixMap f m = m {vector = V.map f (vector m)}

matrixImap :: (V.Unbox a, V.Unbox b) => ((Int, Int) -> a -> b) -> Matrix a -> Matrix b
matrixImap f m = m {vector = V.imap (\i -> f (raiseDims i m)) (vector m)}

-- INCLUSIVE within bounds check.
matrixImapCheckbounds ::
    V.Unbox a => (Int, Int, Int, Int) -> ((Int, Int) -> a -> a) -> Matrix a -> Matrix a
matrixImapCheckbounds bounds f m =
  m
    { vector =
        V.imap
          (\i a ->
             let point = raiseDims i m
              in if withinBounds point bounds
                   then f point a
                   else a)
          (vector m)
    }
  where
    withinBounds (x, y) (lbx, lby, ubx, uby) =
      x >= lbx && y >= lby && x <= ubx && y <= uby

-- left, right, up, down
matrixNeighbours :: V.Unbox a => (Int, Int) -> Matrix a -> [a]
matrixNeighbours (i, j) m =
  map (`matrixGet` m) [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]

matrixUnsafeUpd :: V.Unbox a => Matrix a -> [((Int, Int), a)] -> Matrix a
matrixUnsafeUpd m ls =
  m
    { vector =
        V.unsafeUpd (vector m) (map (\(dims, a) -> (flattenDims dims m, a)) ls)
    }

-- runs op on a vector, giving you a new vector. this is safe
inPlace ::
     forall a. V.Unbox a =>
     (forall s. MV.MVector (MV.PrimState (ST s)) a -> ST s ())
  -> V.Vector a
  -> V.Vector a
inPlace op vec =
  runST $ do
    mv <- V.thaw vec
    op mv
    V.freeze mv

-- TODO: this needs to be moved out of here...
matrixSetBnd :: Int -> Int -> Matrix Double -> Matrix Double
matrixSetBnd n b m =
  let sbHelper mvec =
        let mread dim1 dim2 = do
              res1 <- MV.read mvec (flattenDims dim1 m)
              res2 <- MV.read mvec (flattenDims dim2 m)
              return $ 0.5 * res1 * res2
            mwrite dims = MV.write mvec (flattenDims dims m)
            setBoundVal dimReplace dim1 dim2 =
              mwrite dimReplace =<< mread dim1 dim2
            boundset prevDims bCheck neighbourDims =
              MV.write
                mvec
                (flattenDims prevDims m)
                (if b == bCheck
                   then-1 * matrixGet neighbourDims m
                   else matrixGet neighbourDims m)
         in do
              forM_
                [1 .. n]
                (\i -> do
                   boundset (0, i) 1 (1, i)
                   boundset (n + 1, i) 1 (n, i)
                   boundset (i, 0) 2 (i, 1)
                   boundset (i, n + 1) 2 (i, n))
              setBoundVal (0, 0) (1, 0) (0, 1)
              setBoundVal (0, n + 1) (1, n + 1) (0, n)
              setBoundVal (n + 1, 1) (n, 0) (n + 1, 1)
              setBoundVal (n + 1, n) (n, n + 1) (n + 1, n)
   in m {vector = inPlace sbHelper (vector m)}
