module Graphics.Pylon.Binding.Cairo.Cairo where

import Control.Exception

import Graphics.Pylon.Foreign.Cairo.Surface
import Graphics.Pylon.Foreign.Cairo.Cairo
import Graphics.Pylon.Foreign.Cairo.Types
import Graphics.Pylon.Foreign.Cairo.Error

withCairo :: Surface s -> (Cairo s -> IO b) -> IO b
withCairo (Surface surf) = bracket bra
    (\(Cairo c) -> cairo_destroy c)
  where
    bra = do
        cairo <- cairo_create surf
        st    <- cairo_status cairo
        if st == statusSuccess
            then return $ Cairo cairo
            else throwIO st

setSourceRgb :: Cairo s -> Double -> Double -> Double -> IO ()
setSourceRgb (Cairo c) r g b =
    cairo_set_source_rgb c (realToFrac r) (realToFrac g) (realToFrac b)

setSourceRgba :: Cairo s -> Double -> Double -> Double -> Double -> IO ()
setSourceRgba (Cairo c) r g b a =
    cairo_set_source_rgba c (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)

fill :: Cairo s -> IO ()
fill (Cairo c) = cairo_fill c
