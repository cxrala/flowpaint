module Simulation.Source
  ( Source
  , getSourceFromMouseInput
  ) where

import           Data.List
import Interface.UserInput
    ( MouseInput(mousePos, mouseDown, mousePosLast) )
import           Utils.Fields
import           Utils.Matrix

type Source = ScalarField

-- TODO: taken from line function, figure out which one
line :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
line pa@(xa, ya) pb@(xb, yb) = map maySwitch . unfoldr go $ (x1, y1, 0)
  where
    steep = abs (yb - ya) > abs (xb - xa)
    maySwitch =
      if steep
        then (\(x, y) -> (y, x))
        else id
    [(x1, y1), (x2, y2)] = sort [maySwitch pa, maySwitch pb]
    deltax = x2 - x1
    deltay = abs (y2 - y1)
    ystep =
      if y1 < y2
        then 1
        else -1
    go (xTemp, yTemp, error)
      | xTemp > x2 = Nothing
      | otherwise = Just ((xTemp, yTemp), (xTemp + 1, newY, newError))
      where
        tempError = error + deltay
        (newY, newError) =
          if (2 * tempError) >= deltax
            then (yTemp + ystep, tempError - deltax)
            else (yTemp, tempError)

getSourceFromMouseInput :: MouseInput -> (Int, Int) -> Maybe Source
getSourceFromMouseInput mouseInput dims =
  if mouseDown mouseInput
    then let mPrev = mousePosLast mouseInput
             mCurr = mousePos mouseInput
             linePixels = line mPrev mCurr
          in Just
               (matrixGenerate
                  dims
                  (\x ->
                     if x `elem` linePixels
                       then 1
                       else 0))
    else Nothing
