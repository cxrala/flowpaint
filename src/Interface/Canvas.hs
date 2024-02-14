module Interface.Canvas (
    Canvas(..),
    dimsFromN,
    nFromDims
) where

data Canvas = Canvas {
    canvasScreen :: (Int, Int),
    canvasN :: Int
}

dimsFromN :: Int -> (Int, Int)
dimsFromN n = (n + 2, n + 2)

nFromDims :: (Int, Int) -> Int
nFromDims (x, _) = x - 2