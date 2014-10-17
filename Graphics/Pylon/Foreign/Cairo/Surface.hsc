module Graphics.Pylon.Foreign.Cairo.Surface where

#include <cairo.h>

import Foreign.Ptr
import Foreign.C
import Graphics.Pylon.Foreign.Cairo.Types

type Surface_ = Surface ()
newtype Surface a = Surface (Ptr Surface_)

newtype Content = Content CInt
#{ enum Content, Content
 , contentColor      = CAIRO_CONTENT_COLOR
 , contentAlpha      = CAIRO_CONTENT_ALPHA
 , contentColorAlpha = CAIRO_CONTENT_COLOR_ALPHA
 }

foreign import ccall cairo_surface_create_simular
    :: Ptr Surface_ -> CInt -> CInt -> CInt -> IO (Ptr Surface_)

foreign import ccall cairo_surface_create_simular_image
    :: Ptr Surface_ -> CInt -> CInt -> CInt -> IO (Ptr Surface_)

foreign import ccall cairo_surface_create_for_rectangle
    :: Ptr Surface_ -> CDouble -> CDouble -> CDouble -> CDouble -> IO (Ptr Surface_)

foreign import ccall cairo_surface_reference
    :: Ptr Surface_ -> IO (Ptr Surface_)

foreign import ccall cairo_surface_destroy
    :: Ptr Surface_ -> IO ()

foreign import ccall cairo_surface_status
    :: Ptr Surface_ -> IO CairoStatus

foreign import ccall cairo_surface_finish
    :: Ptr Surface_ -> IO ()

foreign import ccall cairo_surface_flush
    :: Ptr Surface_ -> IO ()

foreign import ccall cairo_surface_get_device
    :: Ptr Surface_ -> IO (Ptr Device)

foreign import ccall cairo_surface_get_font_options
    :: Ptr Surface_ -> Ptr FontOptions -> IO ()

foreign import ccall cairo_surface_get_content
    :: Ptr Surface_ -> IO Content

foreign import ccall cairo_surface_mark_dirty
    :: Ptr Surface_ -> IO ()

foreign import ccall cairo_surface_mark_dirty_rectangle
    :: Ptr Surface_ -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall cairo_surface_set_device_offset
    :: Ptr Surface_ -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_surface_get_device_offset
    :: Ptr Surface_ -> Ptr CDouble -> Ptr CDouble -> IO ()

foreign import ccall cairo_surface_get_device_scale
    :: Ptr Surface_ -> Ptr CDouble -> Ptr CDouble -> IO ()

foreign import ccall cairo_surface_set_device_scale
    :: Ptr Surface_ -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_surface_set_fallback_resolution
    :: Ptr Surface_ -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_surface_get_fallback_resolution
    :: Ptr Surface_ -> Ptr CDouble -> Ptr CDouble -> IO ()

newtype Surface_Type = Surface_Type CInt
#{ enum Surface_Type, Surface_Type
 ,  surfaceTypeImage          = CAIRO_SURFACE_TYPE_IMAGE
 ,  surfaceTypePdf            = CAIRO_SURFACE_TYPE_PDF
 ,  surfaceTypePs             = CAIRO_SURFACE_TYPE_PS
 ,  surfaceTypeXlib           = CAIRO_SURFACE_TYPE_XLIB
 ,  surfaceTypeXcb            = CAIRO_SURFACE_TYPE_XCB
 ,  surfaceTypeGlitz          = CAIRO_SURFACE_TYPE_GLITZ
 ,  surfaceTypeQuartz         = CAIRO_SURFACE_TYPE_QUARTZ
 ,  surfaceTypeWin32          = CAIRO_SURFACE_TYPE_WIN32
 ,  surfaceTypeBeos           = CAIRO_SURFACE_TYPE_BEOS
 ,  surfaceTypeDirectfb       = CAIRO_SURFACE_TYPE_DIRECTFB
 ,  surfaceTypeSvg            = CAIRO_SURFACE_TYPE_SVG
 ,  surfaceTypeOs2            = CAIRO_SURFACE_TYPE_OS2
 ,  surfaceTypeWin32Printing  = CAIRO_SURFACE_TYPE_WIN32_PRINTING
 ,  surfaceTypeQuartzImage    = CAIRO_SURFACE_TYPE_QUARTZ_IMAGE
 ,  surfaceTypeScript         = CAIRO_SURFACE_TYPE_SCRIPT
 ,  surfaceTypeQt             = CAIRO_SURFACE_TYPE_QT
 ,  surfaceTypeRecording      = CAIRO_SURFACE_TYPE_RECORDING
 ,  surfaceTypeVg             = CAIRO_SURFACE_TYPE_VG
 ,  surfaceTypeGl             = CAIRO_SURFACE_TYPE_GL
 ,  surfaceTypeDrm            = CAIRO_SURFACE_TYPE_DRM
 ,  surfaceTypeTee            = CAIRO_SURFACE_TYPE_TEE
 ,  surfaceTypeXml            = CAIRO_SURFACE_TYPE_XML
 ,  surfaceTypeSkia           = CAIRO_SURFACE_TYPE_SKIA
 ,  surfaceTypeSubsurface     = CAIRO_SURFACE_TYPE_SUBSURFACE
 ,  surfaceTypeCogl           = CAIRO_SURFACE_TYPE_COGL
 }

foreign import ccall cairo_surface_get_type
    :: Ptr Surface_ -> IO CInt

foreign import ccall cairo_surface_get_reference_count
    :: Ptr Surface_ -> IO CUInt

foreign import ccall cairo_surface_set_user_data
    :: UserDataSetter Surface_

foreign import ccall cairo_surface_get_user_data
    :: UserDataGetter Surface_

foreign import ccall cairo_surface_copy_page
    :: Ptr Surface_ -> IO ()

foreign import ccall cairo_surface_show_page
    :: Ptr Surface_ -> IO ()

foreign import ccall cairo_surface_has_show_text_glyphs
    :: Ptr Surface_ -> IO CBool

foreign import ccall cairo_surface_set_mime_data
    :: Ptr Surface_ -> CString -> Ptr CUChar -> CULong -> FunPtr (Ptr a -> IO ()) -> Ptr a -> IO CairoStatus

foreign import ccall cairo_surface_get_mime_data
    :: Ptr Surface_ -> CString -> Ptr (Ptr CUChar) -> Ptr CULong -> IO ()

foreign import ccall cairo_surface_supports_mime_type
    :: Ptr Surface_ -> CString -> IO CBool

foreign import ccall cairo_surface_map_to_image
    :: Ptr Surface_ -> Ptr (Rectangle Int) -> IO (Ptr Surface_)

foreign import ccall cairo_surface_unmap_imapge
    :: Ptr Surface_ -> Ptr Surface_ -> IO ()


{-
foreign import ccall cairo_surface_
    :: Ptr Surface_ ->

-}
