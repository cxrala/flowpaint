{-# LANGUAGE DeriveGeneric #-}

import Simulation.State
import Utils.Matrix

import FRP.Yampa
import SDL hiding (Event)
import Codec.Serialise ( readFileDeserialise )
import Interface.RawInput ( RawInput )
import Criterion.Main
import GHC.IO (FilePath)
import Reactivity.SignalFunctions (RenderOutput, signalFunction)
import FRP.Yampa (embed, Event)
import Simulation.State (initialState)
import Reactivity.MouseInput (initialMouse)

import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics

instance NFData a => NFData (Matrix a) where rnf = genericRnf
instance NFData PhysicsConstants where rnf = genericRnf
instance NFData State where rnf = genericRnf
instance NFData RenderOutput where rnf = genericRnf

main :: IO ()
main = do
  eventsSingleStroke <- createTestInput "tests/TestStrokes/single_stroke_1.txt"
  let initialInp = initialInput (700, 700)
  let resolutionSingleStroke = resolutionBenchmark initialInp eventsSingleStroke
  defaultMain [
    bgroup "resolutionBenchmarking" $ map (\res -> bench (show res) $ nf resolutionSingleStroke res) [10, 20 .. 300]
    ]
  where initialInput windowDims = (NoEvent, windowDims)

resolutionBenchmark :: (Event RawInput, (Int, Int)) -> [(DTime, Maybe (Event RawInput, (Int, Int)))] -> Int -> [RenderOutput]
resolutionBenchmark initialInput eventStream canvasSize = 
  simulateSF canvasSize (initialInput, eventStream)

simulateSF :: Int -> ((Event RawInput, (Int, Int)), [(DTime, Maybe (Event RawInput, (Int, Int)))]) -> [RenderOutput]
simulateSF canvasSize =
  let initState = initialState canvasSize
      initMouse = initialMouse in
  embed (signalFunction initState initMouse)

createTestInput :: FilePath -> IO [(DTime, Maybe (Event RawInput, (Int, Int)))]
createTestInput filepath = do
  output <- readFileDeserialise filepath :: IO [(DTime, Maybe (Maybe RawInput, (Int, Int)))]
  return (map deserialisedToSFFormat output)
  where deserialisedToSFFormat (dtime, Nothing) = (dtime, Nothing)
        deserialisedToSFFormat (dtime, Just (mInp, dims)) = (dtime, Just (maybeToEvent mInp, dims))
  

-- main :: IO ()
-- main = do
--   -- let fileName = "./tests/TestStrokes/single_stroke_1.txt"
--   output <- readFileDeserialise "./tests/TestStrokes/single_stroke_1.txt" :: IO [(DTime, Maybe (Maybe RawInput, (Int, Int)))]
--   print output