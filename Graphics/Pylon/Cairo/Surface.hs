module Graphics.Pylon.Cairo.Surface where

import Foreign.Ptr
import Foreign.C.String
import Control.Exception
import Graphics.Pylon.Foreign.Cairo.Surfaces
import Graphics.Pylon.Foreign.Cairo.Surface
import Graphics.Pylon.Foreign.Cairo.Types
import Graphics.Pylon.Foreign.Cairo.Error

withSVGFile :: Maybe FilePath -> Double -> Double -> (Surface SVG -> IO a) -> IO a
withSVGFile file w h = bracket bra
    (\(Surface surf) -> cairo_surface_destroy surf)
  where
    bra = do
        s <- case file of
            Nothing -> 
                cairo_svg_surface_create nullPtr (realToFrac w) (realToFrac h)
            Just f  -> withCString f $ \cf ->
                cairo_svg_surface_create cf (realToFrac w) (realToFrac h)
        st <- cairo_surface_status s
        if st == statusSuccess
            then return $ Surface s
            else throwIO st
