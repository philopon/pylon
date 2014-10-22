module Graphics.Pylon.Foreign.Cairo.Cairo where

#include <cairo.h>

import Foreign.Ptr
import Foreign.C
import Graphics.Pylon.Foreign.Cairo.Surface(Surface, Content(..))
import Graphics.Pylon.Foreign.Cairo.Pattern(Pattern)
import Graphics.Pylon.Foreign.Cairo.Types
    (CBool, UserDataSetter, UserDataGetter)
import Graphics.Pylon.Foreign.Cairo.Error (CairoStatus(..))

newtype Cairo s = Cairo (Ptr (Cairo s))

foreign import ccall cairo_create
    :: Ptr (Surface s) -> IO (Ptr (Cairo s))

foreign import ccall cairo_reference
    :: Ptr (Cairo s) -> IO (Ptr (Cairo s))

foreign import ccall cairo_destroy
    :: Ptr (Cairo s) -> IO ()

foreign import ccall cairo_status
    :: Ptr (Cairo s) -> IO CairoStatus

foreign import ccall cairo_save
    :: Ptr (Cairo s) -> IO ()

foreign import ccall cairo_restore
    :: Ptr (Cairo s) -> IO ()

foreign import ccall cairo_get_target
    :: Ptr (Cairo s) -> IO (Ptr (Surface s))

foreign import ccall cairo_push_group
    :: Ptr (Cairo s) -> IO ()

foreign import ccall cairo_push_group_with_content
    :: Ptr (Cairo s) -> Content -> IO ()

foreign import ccall cairo_pop_group
    :: Ptr (Cairo s) -> IO (Ptr Pattern)

foreign import ccall cairo_pop_group_to_source
    :: Ptr (Cairo s) -> IO ()

foreign import ccall cairo_get_group_target
    :: Ptr (Cairo s) -> IO (Ptr (Surface s))

foreign import ccall cairo_set_source_rgb
    :: Ptr (Cairo s) -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_set_source_rgba
    :: Ptr (Cairo s) -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_set_source
    :: Ptr (Cairo s) -> Ptr Pattern -> IO ()

foreign import ccall cairo_set_source_surface
    :: Ptr (Cairo s) -> Ptr Pattern -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_get_source
    :: Ptr (Cairo s) -> IO (Ptr Pattern)

newtype Antialias = Antialias CInt
#{ enum Antialias, Antialias
 , antialiasDefault  = CAIRO_ANTIALIAS_DEFAULT

 , antialiasNone     = CAIRO_ANTIALIAS_NONE
 , antialiasGray     = CAIRO_ANTIALIAS_GRAY
 , antialiasSubpixel = CAIRO_ANTIALIAS_SUBPIXEL

 , antialiasFast     = CAIRO_ANTIALIAS_FAST
 , antialiasGood     = CAIRO_ANTIALIAS_GOOD
 , antialiasBest     = CAIRO_ANTIALIAS_BEST
 }


foreign import ccall cairo_set_antialias
    :: Ptr (Cairo s) -> Antialias -> IO ()

foreign import ccall cairo_get_antialias
    :: Ptr (Cairo s) -> IO Antialias

foreign import ccall cairo_set_dash
    :: Ptr (Cairo s) -> Ptr CDouble -> CInt -> CDouble -> IO ()

foreign import ccall cairo_get_dash_count
    :: Ptr (Cairo s) -> IO CInt

foreign import ccall cairo_get_dash
    :: Ptr (Cairo s) -> Ptr CDouble -> Ptr CDouble -> IO ()

newtype FillRule = FillRule CInt
#{ enum FillRule, FillRule
 , fillRuleWinding = CAIRO_FILL_RULE_WINDING
 , fillRuleEvenOdd = CAIRO_FILL_RULE_EVEN_ODD
 }

foreign import ccall cairo_set_fill_rule
    :: Ptr (Cairo s) -> FillRule -> IO ()

foreign import ccall cairo_get_fill_rule
    :: Ptr (Cairo s) -> IO FillRule

newtype LineCap = LineCap CInt
#{ enum LineCap, LineCap
 , lineCapButt   = CAIRO_LINE_CAP_BUTT
 , lineCapRound  = CAIRO_LINE_CAP_ROUND
 , lineCapSquare = CAIRO_LINE_CAP_SQUARE
 }

foreign import ccall cairo_set_line_cap
    :: Ptr (Cairo s) -> LineCap -> IO ()

foreign import ccall cairo_get_line_cap
    :: Ptr (Cairo s) -> IO LineCap

newtype LineJoin = LineJoin CInt
#{ enum LineJoin, LineJoin
 , lineJoinMiter = CAIRO_LINE_JOIN_MITER
 , lineJoinRound = CAIRO_LINE_JOIN_ROUND
 , lineJoinBevel = CAIRO_LINE_JOIN_BEVEL
 }

foreign import ccall cairo_set_line_join
    :: Ptr (Cairo s) -> LineJoin -> IO ()

foreign import ccall cairo_get_line_join
    :: Ptr (Cairo s) -> IO LineJoin

foreign import ccall cairo_set_line_width
    :: Ptr (Cairo s) -> CDouble -> IO ()

foreign import ccall cairo_get_line_width
    :: Ptr (Cairo s) -> IO CDouble

foreign import ccall cairo_set_miter_limit
    :: Ptr (Cairo s) -> CDouble -> IO ()

foreign import ccall cairo_get_miter_limit
    :: Ptr (Cairo s) -> IO CDouble

newtype Operator = Operator CInt
#{ enum Operator, Operator
 ,  operatorClear          = CAIRO_OPERATOR_CLEAR

 ,  operatorSource         = CAIRO_OPERATOR_SOURCE
 ,  operatorOver           = CAIRO_OPERATOR_OVER
 ,  operatorIn             = CAIRO_OPERATOR_IN
 ,  operatorOut            = CAIRO_OPERATOR_OUT
 ,  operatorAtop           = CAIRO_OPERATOR_ATOP

 ,  operatorDest           = CAIRO_OPERATOR_DEST
 ,  operatorDestOver       = CAIRO_OPERATOR_DEST_OVER
 ,  operatorDestIn         = CAIRO_OPERATOR_DEST_IN
 ,  operatorDestOut        = CAIRO_OPERATOR_DEST_OUT
 ,  operatorDestAtop       = CAIRO_OPERATOR_DEST_ATOP

 ,  operatorXor            = CAIRO_OPERATOR_XOR
 ,  operatorAdd            = CAIRO_OPERATOR_ADD
 ,  operatorSaturate       = CAIRO_OPERATOR_SATURATE

 ,  operatorMultiply       = CAIRO_OPERATOR_MULTIPLY
 ,  operatorScreen         = CAIRO_OPERATOR_SCREEN
 ,  operatorOverlay        = CAIRO_OPERATOR_OVERLAY
 ,  operatorDarken         = CAIRO_OPERATOR_DARKEN
 ,  operatorLighten        = CAIRO_OPERATOR_LIGHTEN
 ,  operatorColorDodge     = CAIRO_OPERATOR_COLOR_DODGE
 ,  operatorColorBurn      = CAIRO_OPERATOR_COLOR_BURN
 ,  operatorHardLight      = CAIRO_OPERATOR_HARD_LIGHT
 ,  operatorSoftLight      = CAIRO_OPERATOR_SOFT_LIGHT
 ,  operatorDifference     = CAIRO_OPERATOR_DIFFERENCE
 ,  operatorExclusion      = CAIRO_OPERATOR_EXCLUSION
 ,  operatorHslHue         = CAIRO_OPERATOR_HSL_HUE
 ,  operatorHslSaturation  = CAIRO_OPERATOR_HSL_SATURATION
 ,  operatorHslColor       = CAIRO_OPERATOR_HSL_COLOR
 ,  operatorHslLuminosity  = CAIRO_OPERATOR_HSL_LUMINOSITY
 }


foreign import ccall cairo_set_operator
    :: Ptr (Cairo s) -> Operator -> IO ()

foreign import ccall cairo_get_operator
    :: Ptr (Cairo s) -> IO Operator

foreign import ccall cairo_set_tolerance
    :: Ptr (Cairo s) -> CDouble -> IO ()

foreign import ccall cairo_get_tolerance
    :: Ptr (Cairo s) -> IO CDouble

foreign import ccall cairo_clip
    :: Ptr (Cairo s) -> IO ()

foreign import ccall cairo_clip_preserve
    :: Ptr (Cairo s) -> IO ()

foreign import ccall cairo_clip_extents
    :: Ptr (Cairo s) -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_in_clip
    :: Ptr (Cairo s) -> CDouble -> CDouble -> IO CBool

foreign import ccall cairo_reset_clip
    :: Ptr (Cairo s) -> IO ()

-- TODO: RectangleList
data RectangleList

foreign import ccall cairo_rectangle_list_destroy
    :: Ptr RectangleList -> IO ()

foreign import ccall cairo_copy_clip_rectangle_list
    :: Ptr (Cairo s) -> IO (Ptr RectangleList)

foreign import ccall cairo_fill
    :: Ptr (Cairo s) -> IO ()

foreign import ccall cairo_fill_preserve
    :: Ptr (Cairo s) -> IO ()

foreign import ccall cairo_fill_extents
    :: Ptr (Cairo s) -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_in_fill
    :: Ptr (Cairo s) -> CDouble -> CDouble -> IO CBool

foreign import ccall cairo_mask
    :: Ptr (Cairo s) -> Ptr Pattern -> IO ()

foreign import ccall cairo_mask_surface
    :: Ptr (Cairo s) -> Ptr Pattern -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_paint
    :: Ptr (Cairo s) -> IO ()

foreign import ccall cairo_paint_with_alpha
    :: Ptr (Cairo s) -> CDouble -> IO ()

foreign import ccall cairo_stroke
    :: Ptr (Cairo s) -> IO ()

foreign import ccall cairo_stroke_preserve
    :: Ptr (Cairo s) -> IO ()

foreign import ccall cairo_stroke_extents
    :: Ptr (Cairo s) -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_in_stroke
    :: Ptr (Cairo s) -> CDouble -> CDouble -> IO CBool

foreign import ccall cairo_copy_page
    :: Ptr (Cairo s) -> IO ()

foreign import ccall cairo_show_page
    :: Ptr (Cairo s) -> IO ()

foreign import ccall cairo_get_reference_count
    :: Ptr (Cairo s) -> IO CUInt

foreign import ccall cairo_set_user_data
    :: UserDataSetter (Cairo s)

foreign import ccall cairo_get_user_data
    :: UserDataGetter (Cairo s)
