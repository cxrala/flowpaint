-- module Preparation where



-- senseInput :: MVar DTime -> StateVar (V2 CInt) -> Bool -> IO (DTime, Maybe (Event EventPayload, WindowSize))
-- senseInput lastInteraction windowDims _ = do
--     currentTime <- SDL.time
--     (V2 winWidth winHeight) <- get windowDims
--     dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
--     mEvent <- SDL.pollEvent
--     return (dt, fmap (,(fromIntegral winWidth, fromIntegral winHeight)) $ Event . SDL.eventPayload <$> mEvent)