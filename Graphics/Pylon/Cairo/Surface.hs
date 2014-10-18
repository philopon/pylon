module Graphics.Pylon.Cairo.Surface where

import Foreign.C.String
import Control.Exception
import Graphics.Pylon.Foreign.Cairo.Surfaces
import Graphics.Pylon.Foreign.Cairo.Surface
import Graphics.Pylon.Foreign.Cairo.Types

withSVGFile :: FilePath -> Double -> Double -> (Surface SVG -> IO a) -> IO a
withSVGFile file w h = bracket
    (withCString file $ \c -> fmap Surface $
        cairo_svg_surface_create c (realToFrac w) (realToFrac h))
    (\(Surface surf) -> cairo_surface_destroy surf)
