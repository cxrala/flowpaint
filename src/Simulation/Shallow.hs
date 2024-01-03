{-# LANGUAGE OverloadedStrings #-}

module Shallow (
  Shallow  
) where

import qualified Data.Vector as V
import qualified Data.Vector2 as V2
import qualified Data.Vector.Mutable as MV
import Data.
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
data Canvas = Canvas {
    densityField :: Matrix Double,
    velocityField :: Matrix (Double, Double), -- x, y
    canvasSize :: (Int, Int) -- height, width
}

type Source = Matrix Double

defaultCanvas :: (Int, Int) ->  Canvas
defaultCanvas dimensions@(x, y) = Canvas {
    densityField = matrixInitialise dimensions 0,
    velocityField = matrixInitialise dimensions (0, 0),
    canvasSize = dimensions
}

addSource :: Canvas -> Source -> Double -> Canvas
addSource canvas source dt =
    let newField = V.imap (\idx row -> V.imap 
                   (\idy n -> n + dt * (matrixGet source (idx, idy))) row) $ densityField canvas
    in canvas {
        densityField = newField
    }

neighbours :: (Int, Int) -> Matrix a -> [a]
neighbours (i, j) m = map (\x -> matrixGet x m) [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]

-- uses jacobi relaxation instead of gauss-seidel
-- note: should probably change to gauss-seidel if you can figure out how to make it pure.
    -- why? convergence is faster (twice as fast) and also requires less storage.
diffuse :: Canvas -> Int -> Double -> Double -> Canvas
diffuse canvas b diff dt =
    let (height, width) = canvasSize canvas in
    let a = dt * diff * height * width in
    let x0 = densityField canvas in
    let x = matrixInitialise (canvasSize canvas) 0 in
    let sumNeighbours = fold (+) neighbours in
    iterate dif x !! 30 where
        dif field =
            V.imap (\idx row -> V.imap (
                \idy _ -> (matrixGet (idx, idy) x0 + a * (sumNeighbours (idx, idy) x)) / (1 + 4 * a)
            ) row) field

advect :: Canvas -> Int -> Double -> Canvas
advect canvas b dt =
    let dField = densityField canvas in
    let vField = velocityField canvas in
    let (n, m) = canvasSize canvas in 
    let dt0 = dt * n in
    let advected = V.imap (\idx row -> V.imap (
        \idy _ ->
            let (vx, vy) = matrixGet (idx, idy) vField in
            let lb = 0.5 in
            let ubx = n + 0.5 in
            let uby = m + 0.5 in
            let x =
                let estimatedx = idx - dt0 * vx in
                if estimatedx < lb then lb
                else if estimatedx > n + ubx then ubx
                else estimatedx
            in
            let y =
                let estimatedy = idy - dt0 * vy in
                if estimatedy < lb then lb
                else if estimatedy > m + ubx then ubx
                else estimatedy
            in
            let i0 = floor x in
            let i1 = i0 + 1 in
            let j0 = floor y in
            let j1 = j0 + 1 in
            let s1 = x - i0 in
            let s0 = 1 - s1 in
            let t1 = y - j0 in
            let t0 = 1 - t1 in

            s0 * (t0 * matrixGet (i0, j0) dField + t1 * matrixGet (i0, j1) dField) +
            s1 * (t0 * matrixGet (i1, j0) dField + t1 * matrixGet (i1, j1) dField)
            
    ) row) dField