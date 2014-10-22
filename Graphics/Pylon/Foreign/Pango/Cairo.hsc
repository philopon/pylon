module Graphics.Pylon.Foreign.Pango.Cairo where

#include <pango/pangocairo.h>

import Foreign.Ptr
import Graphics.Pylon.Foreign.Cairo.Cairo
import Graphics.Pylon.Foreign.Pango.Layout
import Graphics.Pylon.Foreign.Pango.Font

foreign import ccall pango_cairo_font_map_get_default
    :: IO (Ptr FontMap)

foreign import ccall pango_cairo_create_layout
    :: Ptr (Cairo s) -> IO (Ptr Layout)

foreign import ccall pango_cairo_update_layout
    :: Ptr (Cairo s) -> Ptr Layout -> IO ()

foreign import ccall pango_cairo_show_layout
    :: Ptr (Cairo s) -> Ptr Layout -> IO ()
