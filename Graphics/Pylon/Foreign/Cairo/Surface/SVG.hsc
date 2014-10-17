module Graphics.Pylon.Foreign.Cairo.Surface.SVG where

#include <cairo-svg.h>

import Foreign.Ptr
import Foreign.C
import Graphics.Pylon.Foreign.Cairo.Cairo
import Graphics.Pylon.Foreign.Cairo.Surface
import Graphics.Pylon.Foreign.Cairo.Types

foreign import ccall cairo_svg_surface_create
    :: CString -> CDouble -> CDouble -> IO (Ptr Surface_)

type WriteFunc a = Ptr a -> Ptr CUChar -> CUInt -> IO CairoStatus
foreign import ccall cairo_svg_surface_create_for_stream
    :: FunPtr (WriteFunc a) -> Ptr a -> CDouble -> IO (Ptr Surface_)

newtype SVGVersion = SVGVersion CInt
#{ enum SVGVersion, SVGVersion
 , svgVersion11 = CAIRO_SVG_VERSION_1_1
 , svgVersion12 = CAIRO_SVG_VERSION_1_2
 }

foreign import ccall cairo_svg_surface_restrict_to_version
    :: Ptr Cairo -> CInt -> IO ()

foreign import ccall cairo_svg_get_versions
    :: Ptr (Ptr CInt) -> Ptr CInt -> IO ()

foreign import ccall cairo_svg_version_to_string
    :: CInt -> IO CString
