{-|
Module      : Utility
Description : Utility functions
-}
module Utility where

import Linear
import Linear.Affine
import qualified Data.Map as Map

import Types

-- |This functions filters out all far objects from ObjsMap map
filterFarObjects :: GameInfo        -- ^ GameInfo is used to get tiles width and height
                -> ObjsMap          -- ^ ObjsMap to filter
                -> Point V2 Int     -- ^ Ball position 
                -> ObjsMap          -- ^ Filtered ObjsMap
filterFarObjects gi@(GameInfo _ _ width height _ _ _) objs p@(P (V2 px py)) = 
    Map.filterWithKey (\(ix, iy) _ -> ix <= (px+maxWidth) && ix >= (px-maxWidth) && iy <= (py+maxHeight) && iy >= (py-maxHeight)) objs
    where 
        maxWidth = fromIntegral width*2
        maxHeight = fromIntegral height*2
            
-- |head which returns Nothing on empty list
headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe x = Just $ head x