module Graphics.Pylon.Pango.Cairo where

import Graphics.Pylon.Foreign.Pango.Cairo
import Graphics.Pylon.Foreign.Pango.Layout
import Graphics.Pylon.Foreign.GObject
import Graphics.Pylon.Foreign.Cairo.Cairo
import Foreign.ForeignPtr

createLayout (Cairo c) = fmap Layout $
    pango_cairo_create_layout c
    >>= newForeignPtr p'g_object_unref

updateLayout (Cairo c) (Layout f'l) = withForeignPtr f'l $ \lay ->
    pango_cairo_update_layout c lay

showLayout (Cairo c) (Layout f'l) = withForeignPtr f'l $ \lay ->
    pango_cairo_show_layout c lay
