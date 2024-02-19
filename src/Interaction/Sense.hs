module Interaction.Sense (
    senseInput
)
where

import FRP.Yampa
import SDL hiding (Event)
import Control.Concurrent (MVar)
import Control.Concurrent.MVar (swapMVar)

senseInput :: MVar DTime -> Bool -> IO (DTime, Maybe (Event EventPayload))
senseInput lastInteraction _ = do
    currentTime <- SDL.time
    dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
    mEvent <- SDL.pollEvent
    return (dt, Event . SDL.eventPayload <$> mEvent)