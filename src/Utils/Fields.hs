module Utils.Fields
  ( ScalarField
  , VelocityField
  , elementwiseCombine
  ) where

import           Utils.Matrix
import qualified Data.Vector.Unboxed as VU

type VelocityField = (Matrix Double, Matrix Double)

type ScalarField = Matrix Double

elementwiseCombine :: (VU.Unbox a, VU.Unbox b, VU.Unbox c) => (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
elementwiseCombine f ma mb =
    matrixImap (\idxs a -> f a (matrixGet idxs mb)) ma