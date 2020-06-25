{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( draw
    , basicScene
    , ScreenConfig (ScreenConfig)
    ) where

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Word
import GHC.Float
import Control.Monad
import Control.Monad.ST
import Data.Array.ST


--- SCENE ---
data Scene = Scene { scenePoints :: [Point]
                   , sceneLines :: [(Point, Point)]
                   }
data Tri = Tri { t1 :: Point
               , t2 :: Point
               , t3 :: Point
               }
data Point = Point { px :: Float
                   , py :: Float
                   , pz :: Float
                   , pcolor :: Color
                   }

basicScene :: Scene
basicScene = Scene points lines
    where
        points = map (\x -> pt2d x 300 white) [200, 250, 300, 350, 400, 450]
        lines = [ (pt2d 100 100 white, pt2d 200 120 white)
                , (pt2d 100 100 white, pt2d 200  80 white)
                , (pt2d 100 100 white, pt2d 000 120 white)
                , (pt2d 100 100 white, pt2d 000  80 white)
                , (pt2d 100 100 red, pt2d 120 200 red)
                , (pt2d 100 100 red, pt2d 120 000 red)
                , (pt2d 100 100 red, pt2d  80 200 red)
                , (pt2d 100 100 red, pt2d  80 000 red)
                ]
        pt2d x y = Point (int2Float x) (int2Float y) 0.0
        red = Pixel 255 0 0
        white = Pixel 255 255 255


--- DRAW ---

data ScreenConfig = ScreenConfig { screenWidth :: Int
                                 , screenHeight :: Int
                                 }
data Pixel = Pixel { r :: Word8, g :: Word8, b :: Word8 }
type Color = Pixel

draw :: ScreenConfig -> Scene -> ByteString
draw scfg scn = B.pack $ concatMap p2words $ runST $ do
    let (w, h) = (screenWidth scfg, screenHeight scfg)
    screen <- newArray (0, w * h) (Pixel 0 0 0) :: ST s (STArray s Int Pixel)

    let setPixel x y = writeArray screen (x + w * y)

    forM_ (scenePoints scn) $ \p -> do
        let (x, y) = (float2Int $ px p, float2Int $ py p)
        setPixel x y (pcolor p)

    forM_ (sceneLines scn) $ \(p1, p2) -> do
        let (x1, y1) = (float2Int $ px p1, float2Int $ py p1)
        let (x2, y2) = (float2Int $ px p2, float2Int $ py p2)
        let (dx, dy) = (abs (x2 - x1), abs (y2 - y1))
        let setpx x y = setPixel x y (pcolor p1)
        let (x1', y1', x2', y2') =
                if dx >= dy then (x1, y1, x2, y2) else (y1, x1, y2, x2)
        let (x1'', y1'', x2'', y2'') =
                if x1' <= x2' then (x1', y1', x2', y2') else (x2', y2', x1', y1')
        bresenham (if dx >= dy then setpx else flip setpx) x1'' y1'' x2'' y2''

    getElems screen

    where p2words (Pixel r g b) = [b, g, r, 255] -- convert to BGRA8888

bresenham setpx x1 y1 x2 y2 = forM_ [x1 .. x2] $ \x ->
    let dx = int2Float $ x - x1
    in setpx x $ y1 + float2Int (f * dx)
    where f = int2Float (y2 - y1) / int2Float (x2 - x1)


-- WANT: ByteString constructor that takes an array and a (e -> [Word8]).