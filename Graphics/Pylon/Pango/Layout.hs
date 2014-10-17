module Graphics.Pylon.Pango.Layout where

import Graphics.Pylon.Foreign.Pango.Layout
import Graphics.Pylon.Foreign.Pango.Font

import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import Foreign.ForeignPtr

setText (Layout f'lay) txt =
    withForeignPtr f'lay $ \lay ->
    T.withCStringLen txt $ \(str, len) ->
    pango_layout_set_text lay str (fromIntegral len)

setFontDescription (Layout f'lay) (FontDescription f'fd) =
    withForeignPtr f'lay $ \lay ->
    withForeignPtr f'fd  $ \fd  ->
    pango_layout_set_font_description lay fd
