module Graphics.Pylon.Foreign.Cairo.Surfaces where

#include <cairo.h>

import Foreign.Ptr
import Foreign.C
import Graphics.Pylon.Foreign.Cairo.Cairo
import Graphics.Pylon.Foreign.Cairo.Surface
import Graphics.Pylon.Foreign.Cairo.Types

#ifdef CAIRO_HAS_IMAGE_SURFACE
data Image

foreign import ccall cairo_format_stride_for_width
    :: Format -> CInt -> IO CInt

foreign import ccall cairo_image_surface_create
    :: Format -> CInt -> CInt -> IO (Ptr (Surface Image))

foreign import ccall cairo_image_surface_create_for_data
    :: Ptr CUChar -> Format -> CInt -> CInt -> CInt -> IO (Ptr (Surface Image))

foreign import ccall cairo_image_surface_get_data
    :: Ptr (Surface Image) -> IO (Ptr CUChar)

foreign import ccall cairo_image_surface_get_format
    :: Ptr (Surface Image) -> IO Format

foreign import ccall cairo_image_surface_get_width
    :: Ptr (Surface Image) -> IO CInt

foreign import ccall cairo_image_surface_get_height
    :: Ptr (Surface Image) -> IO CInt

foreign import ccall cairo_image_surface_get_stride
    :: Ptr (Surface Image) -> IO CInt
#endif

#ifdef CAIRO_HAS_PDF_SURFACE
#include <cairo-pdf.h>
data PDF

foreign import ccall cairo_pdf_surface_create
    :: CString -> CDouble -> CDouble -> IO (Ptr (Surface PDF))

foreign import ccall cairo_pdf_surface_create_for_stream
    :: FunPtr (WriteFunc a) -> Ptr a -> CDouble -> CDouble -> IO (Ptr (Surface PDF))

foreign import ccall cairo_pdf_surface_restrict_to_version
    :: Ptr (Surface PDF) -> PDFVersion -> IO ()

newtype PDFVersion = PDFVersion CInt
#{ enum PDFVersion, PDFVersion
 , pdfVersion14 = CAIRO_PDF_VERSION_1_4
 , pdfVersion15 = CAIRO_PDF_VERSION_1_5
 }

foreign import ccall cairo_pdf_get_versions
    :: Ptr (Ptr PDFVersion) -> Ptr CInt -> IO ()

foreign import ccall cairo_pdf_version_to_string
    :: PDFVersion -> IO CString

foreign import ccall cairo_pdf_surface_set_size
    :: Ptr (Surface PDF) -> CDouble -> CDouble -> IO ()
#endif

#ifdef CAIRO_HAS_PNG_FUNCTIONS
foreign import ccall cairo_image_surface_create_from_png
    :: CString -> IO (Ptr (Surface Image))

foreign import ccall cairo_image_surface_create_from_png_stream
    :: FunPtr (ReadFunc a) -> Ptr a -> IO (Ptr (Surface Image))

foreign import ccall cairo_surface_write_to_png
    :: Ptr (Surface Image) -> CString -> IO CairoStatus

foreign import ccall cairo_surface_write_to_png_stream
    :: Ptr (Surface Image) -> FunPtr (WriteFunc a) -> Ptr a -> IO CairoStatus
#endif

#ifdef CAIRO_HAS_PS_SURFACE
#include <cairo-ps.h>
data PS

foreign import ccall cairo_ps_surface_create
    :: CString -> CDouble -> CDouble -> IO (Ptr (Surface PS))

foreign import ccall cairo_ps_surface_create_for_stream
    :: FunPtr (WriteFunc a) -> Ptr a -> CDouble -> CDouble -> IO (Ptr (Surface PS))

newtype PSLevel = PSLevel CInt
#{ enum PSLevel, PSLevel
 , psLevel2 = CAIRO_PS_LEVEL_2
 , psLevel3 = CAIRO_PS_LEVEL_3
 }

foreign import ccall cairo_ps_surface_restrict_to_level
    :: Ptr (Surface PS) -> PSLevel -> IO ()

foreign import ccall cairo_ps_get_levels
    :: Ptr (Ptr PSLevel) -> Ptr CInt -> IO ()

foreign import ccall cairo_ps_level_to_string
    :: PSLevel -> IO CString

foreign import ccall cairo_ps_surface_set_eps
    :: Ptr (Surface PS) -> CBool -> IO ()

foreign import ccall cairo_ps_surface_get_eps
    :: Ptr (Surface PS) -> IO CBool

foreign import ccall cairo_ps_surface_set_size
    :: Ptr (Surface PS) -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_ps_surface_dsc_begin_setup
    :: Ptr (Surface PS) -> IO ()

foreign import ccall cairo_ps_surface_dsc_begin_page_setup
    :: Ptr (Surface PS) -> IO ()

foreign import ccall cairo_ps_surface_dsc_comment
    :: Ptr (Surface PS) -> CString -> IO ()

#endif

#ifdef CAIRO_HAS_RECORDING_SURFACE
data Recording

foreign import ccall cairo_recording_surface_create
    :: Content -> Ptr Rectangle -> IO (Ptr (Surface Recording))

foreign import ccall cairo_recording_surface_ink_extents
    :: Ptr (Surface Recording) -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_recording_surface_get_extents
    :: Ptr (Surface Recording) -> Ptr Rectangle -> IO CBool
#endif

#ifdef CAIRO_HAS_SVG_SURFACE
#include <cairo-svg.h>
data SVG

foreign import ccall cairo_svg_surface_create
    :: CString -> CDouble -> CDouble -> IO (Ptr (Surface SVG))

foreign import ccall cairo_svg_surface_create_for_stream
    :: FunPtr (WriteFunc a) -> Ptr a -> CDouble -> CDouble -> IO (Ptr (Surface SVG))

newtype SVGVersion = SVGVersion CInt
#{ enum SVGVersion, SVGVersion
 , svgVersion11 = CAIRO_SVG_VERSION_1_1
 , svgVersion12 = CAIRO_SVG_VERSION_1_2
 }

foreign import ccall cairo_svg_surface_restrict_to_version
    :: Ptr (Cairo s) -> SVGVersion -> IO ()

foreign import ccall cairo_svg_get_versions
    :: Ptr (Ptr SVGVersion) -> Ptr CInt -> IO ()

foreign import ccall cairo_svg_version_to_string
    :: SVGVersion -> IO CString
#endif
