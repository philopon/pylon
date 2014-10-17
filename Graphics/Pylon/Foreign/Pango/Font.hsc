module Graphics.Pylon.Foreign.Pango.Font where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String

newtype FontDescription = FontDescription (ForeignPtr FontDescription)

foreign import ccall pango_font_description_from_string
    :: CString -> IO (Ptr FontDescription)

foreign import ccall "&pango_font_description_free" p'pango_font_description_free
    :: FunPtr (Ptr FontDescription -> IO ())
