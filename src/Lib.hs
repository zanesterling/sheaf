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
import Data.STRef


--- SCENE ---
data Scene = Scene { scenePoints :: [Point]
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
basicScene = Scene $ map (\x -> Point (int2Float x) 300.0 10.0 red)
                         [200, 250, 300, 350, 400, 450]
    where red = Pixel 255 0 0

--- DRAW ---

data ScreenConfig = ScreenConfig { screenWidth :: Int
                                 , screenHeight :: Int
                                 }
draw :: ScreenConfig -> Scene -> ByteString
draw scfg scn = runST $ do
    screen <- newSTRef $ blankScreen scfg

    forM_ (scenePoints scn) $ modifySTRef screen . drawPoint

    flatpack <$> readSTRef screen
    where drawPoint p = setPixel x y col
            where (x, y) = (float2Int $ px p, float2Int $ py p)
                  col = pcolor p


--- SCREEN ---

type Screen = [[Pixel]]
data Pixel = Pixel { r :: Word8, g :: Word8, b :: Word8 }
type Color = Pixel

blankScreen :: ScreenConfig -> Screen
blankScreen scfg = Prelude.replicate h (Prelude.replicate w blackPixel)
    where w = screenWidth scfg
          h = screenHeight scfg
          blackPixel = Pixel 0 0 0

flatpack :: Screen -> ByteString
flatpack scr = B.pack $ concatMap p2words $ concat scr
    where p2words (Pixel r g b) = [b, g, r, 255] -- convert to BGRA8888

setPixel :: Int -> Int -> Color -> Screen -> Screen
setPixel x y = set2d y x
    where set2d y x col scr = set y (set x col (scr !! y)) scr
          set i x xs = take i xs ++ [x] ++ drop (i+1) xs