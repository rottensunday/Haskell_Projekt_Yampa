module Main where

import FRP.Yampa
import qualified Data.VectorSpace as DD
import Linear
import Linear.Metric
import Control.Monad
import Control.Concurrent (threadDelay)
import Foreign.C.Types
import SDL (($=), Point(..), Rectangle)
import Data.Word
import Data.Text(Text(..), pack)
import Data.Vector3
import qualified SDL
import qualified SDL.Font as SFont
import FRP.Yampa.Delays

import Parser
import Types
import Update

data Texture = Texture SDL.Texture (V2 CInt)

initScreenWidth, initScreenHeight :: CInt
(initScreenWidth, initScreenHeight) = (1280, 960)

initTileWidth, initTileHeight :: CInt
(initTileWidth, initTileHeight) = (32, 32)

spriteSize = V2 initTileWidth initTileHeight
ballClip = SDL.Rectangle (SDL.P (V2 0 0)) spriteSize
wallClip = SDL.Rectangle (SDL.P (V2 initTileWidth 0)) spriteSize

startBall = Ball {
  position = P (V2 640 0),
  velocity = V2 0 0,
  acceleration = V2 0 4.9,
  radius = 10
}

startGameInfo = GameInfo {
  screenWidth = initScreenWidth,
  screenHeight = initScreenHeight,
  tileWidth = initTileWidth,
  tileHeight = initTileHeight
}

loadTexture :: SDL.Renderer -> FilePath -> IO Texture
loadTexture r filePath = do
  surface <- SDL.loadBMP filePath
  size <- SDL.surfaceDimensions surface
  let key = V4 0 maxBound maxBound maxBound
  SDL.surfaceColorKey surface $= Just key
  t <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  return (Texture t size)

loadTextTexture :: SDL.Renderer -> SFont.Font -> String -> V4 Word8 -> IO Texture
loadTextTexture renderer font text color = do
  textSurface <- SFont.solid font color (pack text)
  size <- SDL.surfaceDimensions textSurface
  t <- SDL.createTextureFromSurface renderer textSurface
  SDL.freeSurface textSurface

  return (Texture t size)

renderTexture :: SDL.Renderer -> Texture -> Point V2 CInt -> Maybe (SDL.Rectangle CInt) -> IO ()
renderTexture r (Texture t size) xy clip =
  let dstSize = maybe size (\(SDL.Rectangle _ size') ->  size') clip
  in SDL.copy r t clip (Just (SDL.Rectangle xy dstSize))

main :: IO ()
main = do
  SDL.initializeAll
  SFont.initialize
  font <- SFont.load "D:/NAUKA/Haskell/Projekt/Testy/TestStack/SDL2Yampa1/sdlyampa/resources/arial.ttf" 12
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      (pack "Ball game!")
      SDL.defaultWindow {SDL.windowInitialSize = V2 initScreenWidth initScreenHeight}
  SDL.showWindow window

  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
        { SDL.rendererType = SDL.SoftwareRenderer
        , SDL.rendererTargetTexture = False
        }

  spriteSheetTexture <- loadTexture renderer "D:/NAUKA/Haskell/Projekt/Testy/TestStack/SDL2Yampa1/sdlyampa/resources/sheet1.bmp"

  reactimate parseInput
    (\_ -> threadDelay 10000 >> parseInput >>= (\gi -> return (0.1, Just gi)))
    (\_ output -> appLoop renderer spriteSheetTexture font output)
    (ballController startBall startGameInfo)

  putStrLn "Koniec?"
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit


-- @(p@(P (V2 px py)) v@(V2 vx vy) a@(V2 ax ay)

appLoop :: SDL.Renderer -> Texture -> SFont.Font -> GameOutput -> IO Bool
appLoop renderer sheet font go@(GameOutput b@(Ball p@(P (V2 px py)) v@(V2 vx vy) a@(V2 ax ay) r) end) = do
  events <- SDL.pollEvents
  let textColor = V4 255 0 0 0
  textTexture <- loadTextTexture renderer font (show p ++ "-------------" ++ show v ++ "-------------" ++ show a) textColor
  SDL.rendererDrawColor renderer $= V4 0 0 0 0
  SDL.clear renderer
  renderTexture renderer textTexture (P (V2 100 100)) Nothing
  renderTexture renderer sheet (SDL.P (V2 (round px) (round py))) (Just ballClip)
  renderTexture renderer sheet (SDL.P (V2 608 928)) (Just wallClip)
  SDL.present renderer
  return end
