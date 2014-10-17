module Graphics.Pylon.Cairo.Cairo where

import Control.Exception

import Graphics.Pylon.Foreign.Cairo.Surface
import Graphics.Pylon.Foreign.Cairo.Cairo
import Graphics.Pylon.Foreign.Cairo.Types

withCairo :: Surface a -> (Cairo -> IO b) -> IO b
withCairo (Surface surf) = bracket
    (Cairo `fmap` cairo_create surf)
    (\(Cairo c) -> cairo_destroy c)

setSourceRgb (Cairo c) r g b =
    cairo_set_source_rgb c (realToFrac r) (realToFrac g) (realToFrac b)

setSourceRgba (Cairo c) r g b a =
    cairo_set_source_rgba c (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)

fill :: Cairo -> IO ()
fill (Cairo c) = cairo_fill c
