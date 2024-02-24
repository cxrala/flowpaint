module Interaction.Render
  ( renderState
  , initResources
  ) where

import           Control.Monad             (when)
import           Data.Maybe                (isJust)
import           Data.Ord                  (clamp)
import           Data.Vector.Unboxed       as V
import           Data.Word                 (Word32)
import           Foreign                   (Storable (pokeElemOff))
import           Foreign.Ptr
import           GHC.Float                 (castFloatToWord32, double2Float)
import           Graphics.Rendering.OpenGL as GL hiding (Size)
import           Interface.Canvas          (Canvas (..))
import           Rendering.LoadShaders
import qualified SDL
import           SDL                       (PixelFormat (RGBA8888))
import           Simulation.State          (State (..))
import           Utils.Matrix              (vector)

-- in https://wiki.haskell.org/Yampa/reactimate,
-- corresponds to output/actuate
renderState :: (SDL.Texture, SDL.Renderer) -> Bool -> State -> IO Bool
renderState (texture, renderer) _ state = do
  SDL.clear renderer
  draw state texture
  SDL.present renderer
  return False

draw :: State -> SDL.Texture -> IO ()
draw = updateTexture

updateTexture :: State -> SDL.Texture -> IO ()
updateTexture state texture = do
  let densities = vector $ surfaceLayerDensity state
  (vptr, pitch) <- SDL.lockTexture texture Nothing
  let ptr = castPtr vptr :: Ptr Word32
  V.imapM_ (\i -> pokeElemOff ptr i . mapDensityToFloatingPoint) densities
  SDL.unlockTexture texture

mapDensityToFloatingPoint :: Double -> Word32
mapDensityToFloatingPoint density =
  let d = clamp (0, 1) density
   in castFloatToWord32 $ double2Float d

createTexture :: Canvas -> State -> SDL.Renderer -> IO SDL.Texture
createTexture canvas state renderer = do
  let n = fromIntegral $ canvasN canvas + 2
  texture <-
    SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStreaming
      $ SDL.V2 n n
  updateTexture state texture
  return texture

initResources :: Canvas -> State -> SDL.Renderer -> IO (Program, SDL.Texture)
initResources canvas state renderer = do
  program <-
    loadShaders
      [ ShaderInfo GL.VertexShader (FileSource "shaders/shader.vert")
      , ShaderInfo GL.FragmentShader (FileSource "shaders/shader.frag")
      ]
  GL.currentProgram $= Just program
  texture <- createTexture canvas state renderer
  SDL.glBindTexture texture
  return (program, texture)
