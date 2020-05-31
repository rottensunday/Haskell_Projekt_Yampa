{-# LANGUAGE ScopedTypeVariables #-}
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
import Control.Monad.Trans
import qualified Data.Map.Strict as Map
import qualified SDL
import qualified SDL.Font as SFont
import FRP.Yampa.Delays
import Control.Applicative
import Data.Tiled
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO
import qualified Data.List as L

import Parser
import Types
import Update

initScreenWidth, initScreenHeight :: CInt
(initScreenWidth, initScreenHeight) = (1280, 960)

initTileWidth, initTileHeight :: CInt
(initTileWidth, initTileHeight) = (32, 32)

spriteSize = V2 initTileWidth initTileHeight
ballClip = SDL.Rectangle (SDL.P (V2 0 0)) spriteSize
wallClip = SDL.Rectangle (SDL.P (V2 initTileWidth 0)) spriteSize
spikeClip = SDL.Rectangle (SDL.P (V2 0 initTileHeight)) spriteSize
goalClip = SDL.Rectangle (SDL.P (V2 initTileWidth initTileHeight)) spriteSize

startBall = Ball {
  position = P (V2 657 0),
  velocity = V2 0 0,
  acceleration = V2 0 45,
  radius = 13,
  power = 60
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

prepareGidsToObjsMap :: GidsToObjsMap
prepareGidsToObjsMap = Map.fromList [(1, GameObj Player ballClip), (2, GameObj Wall wallClip), (3, GameObj Spikes spikeClip), (4, GameObj Goal goalClip)]

prepareStaticObjsMap :: FilePath -> IO StaticObjsMap
prepareStaticObjsMap file = do
  board <- loadMapFile file
  let gidsObjsMap = prepareGidsToObjsMap
      tileMap = Map.map (\tile -> gidsObjsMap Map.! tileGid tile) $ (layerData . head . mapLayers) board
  return tileMap



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

  spriteSheetTexture <- loadTexture renderer "D:/NAUKA/Haskell/Projekt/Testy/TestStack/SDL2Yampa1/sdlyampa/resources/sheep3.bmp"
  staticObjs <- prepareStaticObjsMap "../../../../resources/map1.tmx"

  scores <- withFile "wyniki.txt" ReadMode (many . hGetLine) >>= (return . take 3 . L.sort . map (read :: String -> Int))
  reactimate parseInput
    (\_ -> parseInput >>= (\gi -> return (0.015, Just gi)))
    (\_ output -> appLoop renderer spriteSheetTexture scores staticObjs font output)
    (ballController startBall GameInfo {
                              screenWidth = initScreenWidth,
                              screenHeight = initScreenHeight,
                              tileWidth = initTileWidth,
                              tileHeight = initTileHeight,
                              objsMap = staticObjs
                              }
    )

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
  

-- @(p@(P (V2 px py)) v@(V2 vx vy) a@(V2 ax ay)

appLoop :: SDL.Renderer -> Texture -> [Int] -> StaticObjsMap -> SFont.Font -> GameOutput -> IO Bool
appLoop renderer sheet scores objsMap font go@(GameOutput b@(Ball p@(P pV2@(V2 px py)) v@(V2 vx vy) a@(V2 ax ay) r pow) end nshots didwin outtime) = do
  events <- SDL.pollEvents
  mousePos@(P mV2) <- SDL.getAbsoluteMouseLocation
  let textColor = V4 255 0 0 0
  SDL.rendererDrawColor renderer $= V4 0 0 0 0
  SDL.clear renderer

  renderTexture renderer sheet (SDL.P (V2 (round px-15) (round py-15))) (Just ballClip)
  sequence_ $ Map.foldlWithKey
              (\col (posx :: Int, posy :: Int) gameobj -> renderTexture renderer sheet (SDL.P (V2 ((fromIntegral posx :: CInt)*32) ((fromIntegral posy :: CInt)*32))) (Just $ textureCoords gameobj):col)
              []
              objsMap
  SDL.rendererDrawColor renderer $= V4 255 255 255 255
  SDL.drawLine renderer (P (fmap round startDir)) (P (fmap round startDir + lineEnd mV2))
  time <- round . (*1000000) <$> getPOSIXTime
  --textTexture <- loadTextTexture renderer font (show p ++ "-------------" ++ show v ++ "-------------" ++ show a ++ "-------------" ++ show pow) textColor
  --textTexture <- loadTextTexture renderer font (show (time-outtime)) textColor
  textTexture <- loadTextTexture renderer font (show nshots) textColor
  renderTexture renderer textTexture (P (V2 100 100)) Nothing
  SDL.destroyTexture $ texture textTexture
  loadTextTexture renderer font ("High scores: " ++ show scores) textColor >>= (\t -> (renderTexture renderer t (P (V2 100 200)) Nothing) >> (SDL.destroyTexture $ texture t))
  if didwin
    then do
      textTexture <- loadTextTexture renderer font ("YOU WON!! in " ++ show nshots ++ "moves " ++ "Quitting in 4 secs.") textColor
      renderTexture renderer textTexture (P (V2 500 500)) Nothing
      SDL.present renderer
      threadDelay 4000000
      appendFile "wyniki.txt" (show nshots ++ "\n")
      return True
    else do
      SDL.present renderer
      threadDelay (2000 - fromIntegral (time-outtime))
      return end

    where
      startDir = pV2
      dir :: V2 CInt -> V2 Double
      dir mV2 = Linear.Metric.normalize (fmap fromIntegral mV2 - pV2)
      lineEnd :: V2 CInt -> V2 CInt
      lineEnd mV2 = round <$> V2 pow pow * dir mV2
