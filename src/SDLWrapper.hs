{-|
Module      : SDLWrapper
Description : Helper functions for SDL usage
-}
module SDLWrapper where

import Linear
import Foreign.C.Types
import SDL (($=), Point(..), Rectangle)
import Data.Word
import Data.Text(Text(..), pack)
import qualified Data.Map.Strict as Map
import qualified SDL
import qualified SDL.Font as SFont

import Parser
import Types
import GameController

-- |Load a texture from file
loadTexture :: SDL.Renderer
            -> FilePath         -- ^ BMP file path
            -> IO Texture       -- ^ Texture object to return. It's our wrapper around SDL Texture which also contains size of texture
loadTexture r filePath = do
  surface <- SDL.loadBMP filePath
  size <- SDL.surfaceDimensions surface
  let key = V4 0 maxBound maxBound maxBound
  SDL.surfaceColorKey surface $= Just key
  t <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  return (Texture t size)

-- |Create a texture with string (using SDL.Font)
loadTextTexture :: SDL.Renderer 
                -> SFont.Font   -- ^ Font to be used
                -> String       -- ^ Text to be displayed
                -> V4 Word8     -- ^ Text color
                -> IO Texture   -- ^ Texture object to return. It's our wrapper around SDL Texture which also contains size of texture
loadTextTexture renderer font text color = do
  textSurface <- SFont.solid font color (pack text)
  size <- SDL.surfaceDimensions textSurface
  t <- SDL.createTextureFromSurface renderer textSurface
  SDL.freeSurface textSurface
  return (Texture t size)

-- |Render a texture on window
renderTexture :: SDL.Renderer 
              -> Texture                      -- ^ Texture to be drawn
              -> Point V2 CInt                -- ^ Position to draw to
              -> Maybe (SDL.Rectangle CInt)   -- ^ Rectangle from source to cut. If we want whole texture to be drawn, it should be Nothing
              -> IO ()
renderTexture r (Texture t size) xy clip =
  let dstSize = maybe size (\(SDL.Rectangle _ size') ->  size') clip
  in SDL.copy r t clip (Just (SDL.Rectangle xy dstSize))