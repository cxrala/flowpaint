{-# LANGUAGE TupleSections #-}
module Interface.Sense (
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
import System.IO
import Data.IORef (IORef, readIORef, writeIORef)
import Interface.RawInput

type WindowSize = (Int, Int)

-- senseInput :: MVar DTime -> StateVar (V2 CInt) -> Bool -> IO (DTime, Maybe (Event RawInput, WindowSize))
-- senseInput lastInteraction windowDims _ = do
--     currentTime <- SDL.time
--     (V2 winWidth winHeight) <- get windowDims
--     dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
--     mEvent <- SDL.pollEvent
--     return (dt, fmap (,(fromIntegral winWidth, fromIntegral winHeight)) $ Event . parseEventPayload . SDL.eventPayload <$> mEvent)

senseInput :: MVar DTime -> IORef [(DTime, Maybe (Maybe RawInput, WindowSize))] -> StateVar (V2 CInt) -> Bool-> IO (DTime, Maybe (Event RawInput, WindowSize))
senseInput lastInteraction accumListRef windowDims _ = do
    currentTime <- SDL.time
    (V2 winWidth winHeight) <- get windowDims
    dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
    mEvent <- SDL.pollEvent
    accumList <- readIORef accumListRef
    
    contents <- writeIORef accumListRef ((dt, fmap (,(fromIntegral winWidth, fromIntegral winHeight)) $ Just . parseEventPayload . SDL.eventPayload <$> mEvent) : accumList)
    return (dt, fmap (,(fromIntegral winWidth, fromIntegral winHeight)) $ Event . parseEventPayload . SDL.eventPayload <$> mEvent)