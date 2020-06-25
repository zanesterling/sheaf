module Lib
    ( draw
    , basicScene
    , ScreenConfig (ScreenConfig)
    ) where

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Word
import GHC.Float


--- SCENE ---
data Scene = Scene { scenePoint :: Point
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
basicScene = Scene $ Point 200.0 300.0 10.0 $ Pixel 255 0 0

--- DRAW ---

data ScreenConfig = ScreenConfig { screenWidth :: Int
                                 , screenHeight :: Int
                                 }
draw :: ScreenConfig -> Scene -> ByteString
draw scfg scn = flatpack $ etch scn $ blankScreen scfg
    where etch scn scr = setPixel x y (pcolor p) scr
          p = scenePoint scn
          (x, y) = (float2Int $ px p, float2Int $ py p)


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