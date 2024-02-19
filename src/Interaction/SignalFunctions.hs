{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}

module Interaction.SignalFunctions
  ( sf
  ) where

import           FRP.Yampa
import           GHC.Int   (Int32)
import qualified SDL

type WinOutput = String

sf :: SF WinInput WinOutput
sf = parseWinInput >>> toCanvas

-- sf = proc (i, j) -> do
--     inew <- arr fromIntegral -< i
--     jnew <- arr fromIntegral -< j
--     x <- arr show -< (inew, jnew)
--     returnA -< x
parseWinInput :: SF WinInput AppInput
toCanvas :: SF AppInput String -- change to canvas
