module Graphics.Pylon.Cairo.Path where

import Graphics.Pylon.Foreign.Cairo.Types
import Graphics.Pylon.Foreign.Cairo.Path
import Graphics.Pylon.Foreign.Cairo.Cairo

rectangle :: Cairo s -> Double -> Double -> Double -> Double -> IO ()
rectangle (Cairo c) x y w h = do
    cairo_rectangle c (realToFrac x) (realToFrac y) (realToFrac w) (realToFrac h)
