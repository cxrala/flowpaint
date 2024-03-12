{-# LANGUAGE BangPatterns #-}
module Simulation.Source
  ( Source
  , getSourceFromMouseInput
  ) where

import           Data.List
import           Interface.UserInput (MouseInput (mouseDown, mousePos, mousePosLast))
import           Utils.Fields
import           Utils.Matrix
import Simulation.Colors

type Source = (ScalarField, Color)

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

circle :: (RealFrac a1, Integral b, Integral a2, Floating a1, Enum a1) => a1 -> (a2, b) -> [(a2, b)]
circle rad centre@(x, y) =
  map
    (\i ->
       let x1 = floor $ rad * cos (i * pi / 180)
           y1 = floor $ rad * sin (i * pi / 180)
        in (x + x1, y + y1))
    [0, 15 .. 360]

grabPixels rad linePixels =
 concatMap (\x -> linePixels >>= circle x) [0..rad]

getSourceFromMouseInput :: MouseInput -> (Int, Int) -> Maybe Source
getSourceFromMouseInput mouseInput dims =
  let mPrev = mousePosLast mouseInput
      mCurr = mousePos mouseInput in
  if mouseDown mouseInput && mPrev /= mCurr
    then let linePixels = drop 1 $ line mPrev mCurr
             pixels = grabPixels 1 linePixels
            --  pixels = linePixels
          in Just
               (matrixGenerate
                  dims
                  (\x ->
                     if x `elem` pixels
                       then 1
                       else 0), Blue)
    else Nothing
