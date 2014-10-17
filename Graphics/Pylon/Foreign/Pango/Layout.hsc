module Graphics.Pylon.Foreign.Pango.Layout where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String

import Graphics.Pylon.Foreign.Pango.Font

newtype Layout = Layout (ForeignPtr Layout)

foreign import ccall pango_layout_set_text
    :: Ptr Layout -> CString -> CInt -> IO ()

foreign import ccall pango_layout_set_font_description
    :: Ptr Layout -> Ptr FontDescription -> IO ()
