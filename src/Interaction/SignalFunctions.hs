{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}

module Interaction.SignalFunctions
  ( sf
  ) where

import           FRP.Yampa
import           GHC.Int   (Int32)
import qualified SDL
import Interaction.Input
import Data.Functor

type WinOutput = String


sf :: SF WinInput (Maybe String)
sf = parseWinInput >>> toCanvas

toCanvas :: SF AppInput (Maybe String) -- change to canvas
toCanvas = proc i -> do
  returnA -< inpMouseLeft i <&> show