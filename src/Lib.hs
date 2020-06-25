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
data Pixel = Pixel { r :: Word8, g :: Word8, b :: Word8 }
type Color = Pixel

draw :: ScreenConfig -> Scene -> ByteString
draw scfg scn = B.pack $ concatMap p2words $ runST $ do
    let (w, h) = (screenWidth scfg, screenHeight scfg)
    screen <- newArray (0, w * h) (Pixel 0 0 0) :: ST s (STArray s Int Pixel)

    forM_ (scenePoints scn) $ \p -> do
        let (x, y) = (float2Int $ px p, float2Int $ py p)
        setPixel w x y (pcolor p) screen

    getElems screen

    where setPixel w x y col screen = writeArray screen (x + w*y) col
          p2words (Pixel r g b) = [b, g, r, 255] -- convert to BGRA8888

-- WANT: ByteString constructor that takes an array and a (e -> [Word8]).