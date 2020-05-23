{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications, AllowAmbiguousTypes #-}
module Utility where

import FRP.Yampa
import qualified Data.VectorSpace as DD
import Linear
import Linear.Metric
import Control.Monad
import Foreign.C.Types
import SDL (($=), Point(..), Rectangle)
import Data.Word
import Data.Text(Text(..), pack)
import Data.Vector3
import Data.Maybe
import qualified Data.Map as Map
import FRP.Yampa.Delays

import Parser
import Types



filterFarObjects :: StaticObjsMap -> Point V2 Int -> StaticObjsMap
filterFarObjects objs p@(SDL.P (V2 px py)) = Map.filterWithKey (\(ix, iy) _ -> (ix*32) <= (px+64) && (ix*32) >= (px-64) && (iy*32) <= (py+64) && (iy*32) >= (py-64)) objs