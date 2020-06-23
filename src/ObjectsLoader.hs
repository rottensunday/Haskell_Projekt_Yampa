{-|
Module      : ObjectsLoader
Description : Load game data from files
-}
module ObjectsLoader where

import Linear
import Foreign.C.Types
import SDL (Point(..))
import qualified Data.Map.Strict as Map
import Data.Tiled
import Data.List.Split
import System.IO
import System.Directory
import qualified Data.List as L

import Types
import Constants

-- |Make a GidsToObjsMap for all objects which should be considered when loading map
prepareGidsToObjsMap :: GidsToObjsMap
prepareGidsToObjsMap = Map.fromList [(1, GameObj Player ballClip),
                                     (2, GameObj Wall wallClip),
                                     (3, GameObj Spikes spikeClip),
                                     (4, GameObj Goal goalClip)]

-- |Load a map from file and initialize ObjsMap
prepareStaticObjsMapSingle :: (CInt, CInt)  -- ^ tileWidth and tileHeight
                          -> FilePath       -- ^ Map file path
                          -> IO ObjsMap     -- ^ Loaded ObjsMap
prepareStaticObjsMapSingle gi@(width, height) file = do
  board <- loadMapFile file
  let gidsObjsMap = prepareGidsToObjsMap
      tileMap = Map.map (\tile -> gidsObjsMap Map.! tileGid tile) $
        (Map.mapKeys (\k@(x, y) -> (x * fromIntegral width, y * fromIntegral height)) . layerData . head . mapLayers) board
  return tileMap

-- |Load all maps from directory and initialize collection of ObjsMap
prepareStaticObjsMap :: (CInt, CInt)        -- ^ tileWidth and tileHeight
                    -> FilePath             -- ^ Directory with maps (only)
                    -> IO [ObjsMap]         -- ^ Collection of loaded ObjsMaps
prepareStaticObjsMap gi mPath = do
  fileNames <- map (mPath ++) .
                  L.sort .
                  filter (\file -> file /= "." && file /= "..") <$>
                  getDirectoryContents mPath
  mapM (prepareStaticObjsMapSingle gi) fileNames

-- |TODO load dynamic game objects. This is NOT finished: we don't load anything from files.
-- Some objects are hard-coded just to show it works.
prepareDynamicObjs :: Int ->                
                  IO [[DynamicGameObj]]     -- ^ Collection of collections of DynamicGameObjs (one list for each level)
prepareDynamicObjs n = return $ replicate n [DynamicGameObj (GameObj Wall wallClip) (P (V2 200 50)) HorizontalRight 20 200,
                             DynamicGameObj (GameObj Wall wallClip) (P (V2 200 50)) VerticalDown 20 200]

-- |Given directory where maps are, this function creates missing scores files in current directory and
-- returns list of paths to files with scores
prepareScoresFiles :: FilePath              -- ^ Directory where scores should be stored
                  -> IO [FilePath]          -- ^ File paths of all scores files
prepareScoresFiles mPath = do
  fileNames <- map ((++ "wyniki.txt") . head . splitOn ".") . L.sort . filter (\file -> file /= "." && file /= "..") <$> getDirectoryContents mPath
  currentDir <- getCurrentDirectory
  mapM_ (\file -> withFile (currentDir ++ "/" ++ file) AppendMode (const $ pure ())) fileNames -- this should create score files which are not present
  return $ map (\file -> currentDir ++ "/" ++ file) fileNames