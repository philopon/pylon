module Graphics.Pylon.Foreign.Pango.Cairo where

#include <pango/pangocairo.h>

import Foreign.Ptr
import Graphics.Pylon.Foreign.Cairo.Cairo
import Graphics.Pylon.Foreign.Pango.Layout

foreign import ccall pango_cairo_create_layout
    :: Ptr Cairo -> IO (Ptr Layout)

foreign import ccall pango_cairo_update_layout
    :: Ptr Cairo -> Ptr Layout -> IO ()

foreign import ccall pango_cairo_show_layout
    :: Ptr Cairo -> Ptr Layout -> IO ()
