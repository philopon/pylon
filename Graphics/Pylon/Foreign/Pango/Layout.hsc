module Graphics.Pylon.Foreign.Pango.Layout where

#include <pango/pango.h>

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String

import Graphics.Pylon.Foreign.Cairo.Types
import Graphics.Pylon.Foreign.Pango.Font
import Graphics.Pylon.Foreign.Pango.Attribute
import Graphics.Pylon.Foreign.Pango.TabStop

newtype Layout = Layout (ForeignPtr Layout)

foreign import ccall pango_layout_copy
    :: Ptr Layout -> IO (Ptr Layout)

foreign import ccall pango_layout_context_changed
    :: Ptr Layout -> IO ()

foreign import ccall pango_layout_set_text
    :: Ptr Layout -> CString -> CInt -> IO ()

foreign import ccall pango_layout_get_text
    :: Ptr Layout -> IO CString

foreign import ccall pango_layout_get_character_count
    :: Ptr Layout -> IO CInt

foreign import ccall pango_layout_set_markup
    :: Ptr Layout -> CString -> CInt -> IO ()

foreign import ccall pango_layout_set_attributes
    :: Ptr Layout -> Ptr AttrList -> IO ()

foreign import ccall pango_layout_get_attributes
    :: Ptr Layout -> IO (Ptr AttrList)

foreign import ccall pango_layout_set_font_description
    :: Ptr Layout -> Ptr FontDescription -> IO ()

foreign import ccall pango_layout_get_font_description
    :: Ptr Layout -> IO (Ptr FontDescription)

foreign import ccall pango_layout_set_width
    :: Ptr Layout -> CInt -> IO ()

foreign import ccall pango_layout_get_width
    :: Ptr Layout -> IO CInt

foreign import ccall pango_layout_set_height
    :: Ptr Layout -> CInt -> IO ()

foreign import ccall pango_layout_get_height
    :: Ptr Layout -> IO CInt

newtype WrapMode = WrapMode CInt
#{ enum WrapMode, WrapMode
 , wrapWord     = PANGO_WRAP_WORD
 , wrapChar     = PANGO_WRAP_CHAR
 , wrapWordChar = PANGO_WRAP_WORD_CHAR
 }

foreign import ccall pango_layout_set_wrap
    :: Ptr Layout -> WrapMode -> IO ()

foreign import ccall pango_layout_get_wrap
    :: Ptr Layout -> IO WrapMode

foreign import ccall pango_layout_is_wrapped
    :: Ptr Layout -> IO CBool

newtype EllipsizeMode = EllipsizeMode CInt
#{ enum EllipsizeMode, EllipsizeMode
 , ellipsizeNone   = PANGO_ELLIPSIZE_NONE
 , ellipsizeStart  = PANGO_ELLIPSIZE_START
 , ellipsizeMiddle = PANGO_ELLIPSIZE_MIDDLE
 , ellipsizeEnd    = PANGO_ELLIPSIZE_END
 }

foreign import ccall pango_layout_set_ellipsize
    :: Ptr Layout -> EllipsizeMode -> IO ()

foreign import ccall pango_layout_get_ellipsize
    :: Ptr Layout -> IO EllipsizeMode

foreign import ccall pango_layout_is_ellipsized
    :: Ptr Layout -> IO CBool

foreign import ccall pango_layout_set_indent
    :: Ptr Layout -> CInt -> IO ()

foreign import ccall pango_layout_get_indent
    :: Ptr Layout -> IO CInt

foreign import ccall pango_layout_get_spacing
    :: Ptr Layout -> IO CInt

foreign import ccall pango_layout_set_spacing
    :: Ptr Layout -> CInt -> IO ()

foreign import ccall pango_layout_set_justify
    :: Ptr Layout -> CBool -> IO ()

foreign import ccall pango_layout_get_justify
    :: Ptr Layout -> IO CInt

foreign import ccall pango_layout_set_auto_dir
    :: Ptr Layout -> CBool -> IO ()

foreign import ccall pango_layout_get_auto_dir
    :: Ptr Layout -> IO CInt

newtype Alignment = Alignment CInt
#{ enum Alignment, Alignment
 , alignLeft   = PANGO_ALIGN_LEFT
 , alignCenter = PANGO_ALIGN_CENTER
 , alignRight  = PANGO_ALIGN_RIGHT
 }

foreign import ccall pango_layout_set_alignment
    :: Ptr Layout -> Alignment -> IO ()

foreign import ccall pango_layout_get_alignment
    :: Ptr Layout -> IO Alignment

foreign import ccall pango_layout_set_tabs
    :: Ptr Layout -> Ptr TabArray -> IO ()

foreign import ccall pango_layout_get_tabs
    :: Ptr Layout -> IO (Ptr TabArray)

foreign import ccall pango_layout_set_single_paragraph_mode
    :: Ptr Layout -> CBool -> IO ()

foreign import ccall pango_layout_get_single_paragraph_mode
    :: Ptr Layout -> IO CBool

foreign import ccall pango_layout_get_unknown_glyphs_count
    :: Ptr Layout -> IO CInt
