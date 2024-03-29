module Interaction.Render
  ( renderState
  , initResources
  ) where

import           Control.Monad             (when)
import           Data.Maybe                (isJust)
import           Data.Ord                  (clamp)
import           Data.Vector.Unboxed       as V
import           Data.Word                 (Word32, Word8)
import           Foreign                   (Storable (pokeElemOff, peekElemOff))
import           Foreign.Ptr
import           GHC.Float                 (castFloatToWord32, double2Float)
import           Graphics.Rendering.OpenGL as GL hiding (Size)
import qualified SDL
import           SDL                       (PixelFormat (RGBA8888))
import           Simulation.State          (State (..))
import           Utils.Matrix              (vector)
import qualified SDL.Internal.Exception as SDL
import Control.Exception (try, SomeException (..))
import Foreign.C (CString)
import SDL.Internal.Exception (getError)

-- in https://wiki.haskell.org/Yampa/reactimate,
-- corresponds to output/actuate
renderState :: (SDL.Texture, SDL.Renderer) -> Bool -> State -> IO Bool
renderState (texture, renderer) _ state = do
  SDL.rendererDrawColor renderer $= SDL.V4 255 255 255 255
  SDL.clear renderer
  draw state texture renderer
  SDL.copy renderer texture Nothing Nothing
  SDL.present renderer
  return False

draw :: State -> SDL.Texture -> SDL.Renderer -> IO ()
draw = updateTexture

updateTexture :: State -> SDL.Texture -> SDL.Renderer -> IO ()
updateTexture state texture renderer = do
  let densities = vector $ surfaceLayerDensity state
  (vptr, pitch) <- SDL.lockTexture texture Nothing
  let ptr = castPtr vptr :: Ptr (SDL.V4 Word8)
  V.imapM_ (\i -> pokeElemOff ptr i . mapDensityToRGBA) densities
  SDL.unlockTexture texture

mapDensityToRGBA :: Double -> SDL.V4 Word8
mapDensityToRGBA density =
  let d = floor $ if density >= 1 then 255 else clamp (0, 1) density * 256
      dPresent = if d > 0 then 255 else 0
   in SDL.V4 0 0 d dPresent

createTexture :: State -> SDL.Renderer -> IO SDL.Texture
createTexture state renderer = do
  let n = fromIntegral $ canvasSize state + 2
  texture <-
    SDL.createTexture renderer SDL.ABGR8888 SDL.TextureAccessStreaming -- note: abgr => rgba (mac little endian)
      $ SDL.V2 n n
  updateTexture state texture renderer
  return texture

initResources :: State -> SDL.Renderer -> IO SDL.Texture
initResources state renderer = do
  texture <- createTexture state renderer
  SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend
  SDL.textureBlendMode texture $= SDL.BlendAlphaBlend
  return texture
