module Graphics.Pylon.Foreign.Cairo.Pattern where

#include <cairo.h>

import Graphics.Pylon.Foreign.Cairo.Error(CairoStatus(..))
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C

newtype Pattern = Pattern (ForeignPtr Pattern)

foreign import ccall cairo_pattern_add_color_stop_rgb
    :: Ptr Pattern -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_pattern_add_color_stop_rgba
    :: Ptr Pattern -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_pattern_get_color_stop_count
    :: Ptr Pattern -> Ptr CInt -> IO CairoStatus

foreign import ccall cairo_pattern_get_color_stop_rgba
    :: Ptr Pattern -> CInt -> Ptr CDouble
    -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CairoStatus

foreign import ccall cairo_pattern_create_rgb
    :: CDouble -> CDouble -> CDouble -> IO (Ptr Pattern)

foreign import ccall cairo_pattern_create_rgba
    :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Ptr Pattern)

foreign import ccall cairo_pattern_get_rgba
    :: Ptr Pattern -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CairoStatus


{-
foreign import ccall cairo_pattern_
    :: Ptr Pattern -> 
-}
