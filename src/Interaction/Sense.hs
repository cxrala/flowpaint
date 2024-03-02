{-# LANGUAGE TupleSections #-}
module Interaction.Sense (
    senseInput
)
where

import FRP.Yampa
import SDL hiding (Event)
import Control.Concurrent (MVar)
import Control.Concurrent.MVar (swapMVar)
import Graphics.Rendering.OpenGL (StateVar)
import Foreign.C (CInt)
import Simulation.State (State)

type WindowSize = (Int, Int)

senseInput :: MVar DTime -> StateVar (V2 CInt) -> Bool -> IO (DTime, Maybe (Event EventPayload, WindowSize))
senseInput lastInteraction windowDims _ = do
    currentTime <- SDL.time
    (V2 winWidth winHeight) <- get windowDims
    dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
    mEvent <- SDL.pollEvent
    return (dt, fmap (,(fromIntegral winWidth, fromIntegral winHeight)) $ Event . SDL.eventPayload <$> mEvent)