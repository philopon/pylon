module Graphics.Pylon.Foreign.Cairo.Cairo where

#include <cairo.h>

import Foreign.Ptr
import Foreign.C
import Graphics.Pylon.Foreign.Cairo.Surface
import Graphics.Pylon.Foreign.Cairo.Types

newtype Cairo = Cairo (Ptr Cairo)

foreign import ccall cairo_create
    :: Ptr Surface_ -> IO (Ptr Cairo)

foreign import ccall cairo_reference
    :: Ptr Cairo -> IO (Ptr Cairo)

foreign import ccall cairo_destroy
    :: Ptr Cairo -> IO CInt

foreign import ccall cairo_status
    :: Ptr Cairo -> IO (Ptr CairoStatus)

foreign import ccall cairo_save
    :: Ptr Cairo -> IO ()

foreign import ccall cairo_restore
    :: Ptr Cairo -> IO ()

foreign import ccall cairo_get_target
    :: Ptr Cairo -> IO (Ptr Surface_)

foreign import ccall cairo_push_group
    :: Ptr Cairo -> IO ()

foreign import ccall cairo_push_group_with_content
    :: Ptr Cairo -> Ptr Content -> IO ()

foreign import ccall cairo_pop_group
    :: Ptr Cairo -> IO (Ptr Pattern)

foreign import ccall cairo_pop_group_to_source
    :: Ptr Cairo -> IO ()

foreign import ccall cairo_get_group_target
    :: Ptr Cairo -> IO (Ptr Surface_)

foreign import ccall cairo_set_source_rgb
    :: Ptr Cairo -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_set_source_rgba
    :: Ptr Cairo -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_set_source
    :: Ptr Cairo -> Ptr Pattern -> IO ()

foreign import ccall cairo_set_source_surface
    :: Ptr Cairo -> Ptr Pattern -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_get_source
    :: Ptr Cairo -> IO (Ptr Pattern)

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
    :: Ptr Cairo -> CInt -> IO ()

foreign import ccall cairo_get_antialias
    :: Ptr Cairo -> IO CInt

foreign import ccall cairo_set_dash
    :: Ptr Cairo -> Ptr CDouble -> CInt -> CDouble -> IO ()

foreign import ccall cairo_get_dash_count
    :: Ptr Cairo -> IO CInt

foreign import ccall cairo_get_dash
    :: Ptr Cairo -> Ptr CDouble -> Ptr CDouble -> IO ()

newtype FillRule = FillRule CInt
#{ enum FillRule, FillRule
 , fillRuleWinding = CAIRO_FILL_RULE_WINDING
 , fillRuleEvenOdd = CAIRO_FILL_RULE_EVEN_ODD
 }

foreign import ccall cairo_set_fill_rule
    :: Ptr Cairo -> CInt -> IO ()

foreign import ccall cairo_get_fill_rule
    :: Ptr Cairo -> IO CInt

newtype LineCap = LineCap CInt
#{ enum LineCap, LineCap
 , lineCapButt   = CAIRO_LINE_CAP_BUTT
 , lineCapRound  = CAIRO_LINE_CAP_ROUND
 , lineCapSquare = CAIRO_LINE_CAP_SQUARE
 }


foreign import ccall cairo_set_line_cap
    :: Ptr Cairo -> CInt -> IO ()

foreign import ccall cairo_get_line_cap
    :: Ptr Cairo -> IO CInt

newtype LineJoin = LineJoin CInt
#{ enum LineJoin, LineJoin
 , lineJoinMiter = CAIRO_LINE_JOIN_MITER
 , lineJoinRound = CAIRO_LINE_JOIN_ROUND
 , lineJoinBevel = CAIRO_LINE_JOIN_BEVEL
 }

foreign import ccall cairo_set_line_join
    :: Ptr Cairo -> CInt -> IO ()

foreign import ccall cairo_get_line_join
    :: Ptr Cairo -> IO CInt

foreign import ccall cairo_set_line_width
    :: Ptr Cairo -> CDouble -> IO ()

foreign import ccall cairo_get_line_width
    :: Ptr Cairo -> IO CDouble

foreign import ccall cairo_set_mitter_limit
    :: Ptr Cairo -> CDouble -> IO ()

foreign import ccall cairo_get_mitter_limit
    :: Ptr Cairo -> IO CDouble

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
    :: Ptr Cairo -> CInt -> IO ()

foreign import ccall cairo_get_operator
    :: Ptr Cairo -> IO CInt

foreign import ccall cairo_set_tolerance
    :: Ptr Cairo -> CDouble -> IO ()

foreign import ccall cairo_get_tolerance
    :: Ptr Cairo -> IO CDouble

foreign import ccall cairo_clip
    :: Ptr Cairo -> IO ()

foreign import ccall cairo_clip_preserve
    :: Ptr Cairo -> IO ()

foreign import ccall cairo_clip_extents
    :: Ptr Cairo -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_in_clip
    :: Ptr Cairo -> CDouble -> CDouble -> IO CBool

foreign import ccall cairo_reset_clip
    :: Ptr Cairo -> IO ()

data RectangleList

foreign import ccall cairo_rectangle_list_destroy
    :: Ptr RectangleList -> IO ()

foreign import ccall cairo_copy_clip_rectangle_list
    :: Ptr Cairo -> IO (Ptr RectangleList)

foreign import ccall cairo_fill
    :: Ptr Cairo -> IO ()

foreign import ccall cairo_fill_preserve
    :: Ptr Cairo -> IO ()

foreign import ccall cairo_fill_extents
    :: Ptr Cairo -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_in_fill
    :: Ptr Cairo -> CDouble -> CDouble -> IO CBool

foreign import ccall cairo_mask
    :: Ptr Cairo -> Ptr Pattern -> IO ()

foreign import ccall cairo_mask_surface
    :: Ptr Cairo -> Ptr Pattern -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_paint
    :: Ptr Cairo -> IO ()

foreign import ccall cairo_paint_with_alpha
    :: Ptr Cairo -> CDouble -> IO ()

foreign import ccall cairo_stroke
    :: Ptr Cairo -> IO ()

foreign import ccall cairo_stroke_preserve
    :: Ptr Cairo -> IO ()

foreign import ccall cairo_stroke_extents
    :: Ptr Cairo -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall cairo_in_stroke
    :: Ptr Cairo -> CDouble -> CDouble -> IO CBool

foreign import ccall cairo_copy_page
    :: Ptr Cairo -> IO ()

foreign import ccall cairo_show_page
    :: Ptr Cairo -> IO ()

foreign import ccall cairo_get_reference_count
    :: Ptr Cairo -> IO CUInt

foreign import ccall cairo_set_user_data
    :: UserDataSetter Cairo

foreign import ccall cairo_get_user_data
    :: UserDataGetter Cairo
