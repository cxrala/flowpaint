module Interaction.Render
  ( renderState
  , initResources
  ) where

import           Control.Exception         (SomeException (..), try)
import           Control.Monad             (when)
import           Data.Maybe                (isJust)
import           Data.Ord                  (clamp)
import           Data.Vector.Unboxed       as V
import           Data.Word                 (Word32, Word8)
import           Foreign                   (Storable (peekElemOff, pokeElemOff))
import           Foreign.C                 (CString)
import           Foreign.Ptr
import           GHC.Float                 (castFloatToWord32, double2Float)
import           Graphics.Rendering.OpenGL as GL hiding (Size)
import qualified SDL
import           SDL                       (PixelFormat (RGBA8888))
import qualified SDL.Internal.Exception    as SDL
import           SDL.Internal.Exception    (getError)
import           Simulation.State          (State (..))
import           Utils.Matrix              (vector)
import Interaction.SignalFunctions (RenderOutput (QuitCanvas, CanvasState))
import Utils.Fields (ScalarField)

-- in https://wiki.haskell.org/Yampa/reactimate,
-- corresponds to output/actuate
renderState :: (SDL.Texture, SDL.Renderer) -> Bool -> RenderOutput -> IO Bool
renderState (texture, renderer) _ state =
  case state of
    QuitCanvas -> return True
    CanvasState (canvasState, waterToggled) -> do
      SDL.rendererDrawColor renderer $= SDL.V4 255 255 255 255
      SDL.clear renderer
      updateTexture (surfaceLayerDensity canvasState) mapPigmentDensityToRGBA texture renderer
      SDL.copy renderer texture Nothing Nothing
      when waterToggled $ do
        updateTexture (capillaryLayerDensity canvasState) mapWaterDensityToRGBA texture renderer
        SDL.copy renderer texture Nothing Nothing
      SDL.present renderer
      return False

updateTexture :: ScalarField -> (Double -> SDL.V4 Word8) -> SDL.Texture -> SDL.Renderer -> IO ()
updateTexture matrix rgbaMap texture renderer = do
  let densities = vector matrix
  (vptr, pitch) <- SDL.lockTexture texture Nothing
  let ptr = castPtr vptr :: Ptr (SDL.V4 Word8)
  V.imapM_ (\i -> pokeElemOff ptr i . rgbaMap) densities
  SDL.unlockTexture texture

mapPigmentDensityToRGBA :: Double -> SDL.V4 Word8
mapPigmentDensityToRGBA density =
  let alphaval =
        floor
          $ if density >= 1
              then 255
              else clamp (0, 1) density * 256
   in SDL.V4 0 0 139 alphaval

mapWaterDensityToRGBA :: Double -> SDL.V4 Word8
mapWaterDensityToRGBA density =
  let alphaval = if density > 0 then 100 else 0 in
    SDL.V4 212 241 249 alphaval

createTexture :: State -> SDL.Renderer -> IO SDL.Texture
createTexture state renderer = do
  let n = fromIntegral $ canvasSize state + 2
  SDL.createTexture renderer SDL.ABGR8888 SDL.TextureAccessStreaming -- note: abgr => rgba (mac little endian)
      $ SDL.V2 n n

initResources :: State -> SDL.Renderer -> IO SDL.Texture
initResources state renderer = do
  texture <- createTexture state renderer
  SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend
  SDL.textureBlendMode texture $= SDL.BlendAlphaBlend
  return texture
