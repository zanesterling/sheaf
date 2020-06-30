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

f2i = float2Int
i2f = int2Float


--- SCENE ---
data Scene = Scene { scenePoints    :: [Point]
                   , sceneLines     :: [(Point, Point)]
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
        lines = [ (pt2d 100 100 red, pt2d 200 120 white)
                , (pt2d 100 100 red, pt2d 200  80 white)
                , (pt2d 100 100 red, pt2d 000 120 white)
                , (pt2d 100 100 red, pt2d 000  80 white)
                , (pt2d 100 100 red, pt2d 120 200 white)
                , (pt2d 100 100 red, pt2d 120 000 white)
                , (pt2d 100 100 red, pt2d  80 200 white)
                , (pt2d 100 100 red, pt2d  80 000 white)
                ]
        pt2d x y = Point (i2f x) (i2f y) 0.0
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
    let setPixel' fx fy = setPixel (f2i fx) (f2i fy)

    forM_ (scenePoints scn) $ \(Point fx fy _ col) -> setPixel' fx fy col

    forM_ (sceneLines scn) (drawLine setPixel)

    getElems screen

    where p2words (Pixel r g b) = [b, g, r, 255] -- convert to BGRA8888

type Pen s = Int -> Int -> Color -> ST s ()


drawPoint :: Pen s -> Point -> ST s ()
drawPoint setpx (Point x y _ col) = setpx (f2i x) (f2i y) col


type Line = (Point, Point)
drawLine :: Pen s -> Line -> ST s ()
drawLine setPixel (Point fx1 fy1 _ c1, Point fx2 fy2 _ c2) = do
    let (x1, y1, x2, y2) = (f2i fx1, f2i fy1, f2i fx2, f2i fy2)
    let (dx, dy) = (abs (x2 - x1), abs (y2 - y1))
    let (x1', y1', x2', y2') =
            if dx >= dy then (x1, y1, x2, y2) else (y1, x1, y2, x2)
    let (x1'', y1'', c1', x2'', y2'', c2') =
            if x1' <= x2' then (x1', y1', c1, x2', y2', c2) else (x2', y2', c2, x1', y1', c1)
    let setpx = if dx >= dy then setPixel else flip setPixel
        p1' = (x1'', y1'', c1')
        p2' = (x2'', y2'', c2')
    bresenham setpx p1' p2'

bresenham :: Pen s -> (Int, Int, Color) -> (Int, Int, Color) -> ST s ()
bresenham setpx (x1, y1, c1) (x2, y2, c2) =
    forM_ [x1 .. x2] $ \x ->
        let dx = i2f $ x - x1
            p = dx / i2f (x2 - x1)
            f = i2f (y2 - y1) / i2f (x2 - x1)
        in setpx x (y1 + f2i (f * dx)) (lerp c1 c2 p)


-- WANT: ByteString constructor that takes an array and a (e -> [Word8]).

class Lerpy l where lerp :: l -> l -> Float -> l

w2f = i2f . fromIntegral
f2w = fromIntegral . f2i

instance Lerpy Float where lerp x y p = x*(p-1) + y*p
instance Lerpy Word8 where
    lerp x y p = f2w ((w2f x)*(1-p) + (w2f y)*p)
instance Lerpy Pixel where
    lerp (Pixel r1 g1 b1) (Pixel r2 g2 b2) p =
        Pixel (lerp r1 r2 p) (lerp g1 g2 p) (lerp b1 b2 p)
instance Lerpy Point where
    lerp (Point x1 y1 z1 c1) (Point x2 y2 z2 c2) p =
        Point (lerp x1 x2 p) (lerp y1 y2 p) (lerp z1 z2 p) (lerp c1 c2 p)