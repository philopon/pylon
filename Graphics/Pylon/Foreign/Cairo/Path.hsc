module Graphics.Pylon.Foreign.Cairo.Path where

#include <cairo.h>

import Foreign.Ptr
import Foreign.C
import Graphics.Pylon.Foreign.Cairo.Cairo
import Graphics.Pylon.Foreign.Cairo.Types

data Path

foreign import ccall cairo_copy_path
    :: Ptr (Cairo s) -> IO (Ptr Path)

foreign import ccall cairo_path_flat
    :: Ptr (Cairo s) -> IO (Ptr Path)

foreign import ccall cairo_path_destroy
    :: Ptr Path -> IO ()

foreign import ccall cairo_append_path
    :: Ptr (Cairo s) -> Ptr Path -> IO ()


foreign import ccall cairo_has_current_point
    :: Ptr (Cairo s) -> IO CBool

foreign import ccall cairo_get_current_point
    :: Ptr (Cairo s) -> Ptr CDouble -> Ptr CDouble -> IO ()

foreign import ccall cairo_new_path
    :: Ptr (Cairo s) -> IO ()

foreign import ccall cairo_new_sub_path
    :: Ptr (Cairo s) -> IO ()

foreign import ccall cairo_close_path
    :: Ptr (Cairo s) -> IO ()

foreign import ccall cairo_arc
    :: Ptr (Cairo s) -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_arc_negative
    :: Ptr (Cairo s) -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_curve_to
    :: Ptr (Cairo s) -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_line_to
    :: Ptr (Cairo s) -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_move_to
    :: Ptr (Cairo s) -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_rectangle
    :: Ptr (Cairo s) -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_rel_curve_to
    :: Ptr (Cairo s) -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_rel_line_to
    :: Ptr (Cairo s) -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_rel_move_to
    :: Ptr (Cairo s) -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_path_extents
    :: Ptr (Cairo s) -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
