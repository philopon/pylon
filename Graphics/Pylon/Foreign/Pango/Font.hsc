module Graphics.Pylon.Foreign.Pango.Font where

#include <pango/pango.h>

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String
import Graphics.Pylon.Foreign.Cairo.Types(CBool)

newtype FontDescription = FontDescription (ForeignPtr FontDescription)

foreign import ccall pango_font_description_new
    :: IO (Ptr FontDescription)

foreign import ccall pango_font_description_copy
    :: Ptr FontDescription -> IO (Ptr FontDescription)

foreign import ccall pango_font_description_equal
    :: Ptr FontDescription -> Ptr FontDescription -> IO CBool

foreign import ccall pango_font_description_free
    :: Ptr FontDescription -> IO ()

foreign import ccall "&pango_font_description_free" p'pango_font_description_free
    :: FunPtr (Ptr FontDescription -> IO ())

foreign import ccall pango_font_description_set_family
    :: Ptr FontDescription -> CString -> IO ()

foreign import ccall pango_font_description_get_family
    :: Ptr FontDescription -> IO CString

newtype Style = Style CInt
#{ enum Style, Style
 , styleNormal  = PANGO_STYLE_NORMAL
 , styleOblique = PANGO_STYLE_OBLIQUE
 , styleItalic  = PANGO_STYLE_ITALIC
 }

foreign import ccall pango_font_description_set_style
    :: Ptr FontDescription -> Style -> IO ()

foreign import ccall pango_font_description_get_style
    :: Ptr FontDescription -> IO Style

newtype Variant = Variant CInt
#{ enum Variant, Variant
 , variantNormal    = PANGO_VARIANT_NORMAL
 , variantSmallCaps = PANGO_VARIANT_SMALL_CAPS
 }

foreign import ccall pango_font_description_set_variant
    :: Ptr FontDescription -> Variant -> IO ()

foreign import ccall pango_font_description_get_variant
    :: Ptr FontDescription -> IO Variant

newtype Weight = Weight CInt
#{ enum Weight, Weight
 , weightThin       = PANGO_WEIGHT_THIN
 , weightUltraLight = PANGO_WEIGHT_ULTRALIGHT
 , weightLight      = PANGO_WEIGHT_LIGHT
 , weightSemiLight  = PANGO_WEIGHT_SEMILIGHT
 , weightBook       = PANGO_WEIGHT_BOOK
 , weightNormal     = PANGO_WEIGHT_NORMAL
 , weightMedium     = PANGO_WEIGHT_MEDIUM
 , weightSemiBold   = PANGO_WEIGHT_SEMIBOLD
 , weightBold       = PANGO_WEIGHT_BOLD
 , weightUltraBold  = PANGO_WEIGHT_ULTRABOLD
 , weightHeavy      = PANGO_WEIGHT_HEAVY
 , weightUltraHeavy = PANGO_WEIGHT_ULTRAHEAVY
 }

foreign import ccall pango_font_description_set_weight
    :: Ptr FontDescription -> Weight -> IO ()

foreign import ccall pango_font_description_get_weight
    :: Ptr FontDescription -> IO Weight

newtype Stretch = Stretch CInt
#{ enum Stretch, Stretch
 , stretchUltraCondensed = PANGO_STRETCH_ULTRA_CONDENSED
 , stretchExtraCondensed = PANGO_STRETCH_EXTRA_CONDENSED
 , stretchCondensed      = PANGO_STRETCH_CONDENSED
 , stretchSemiCondensed  = PANGO_STRETCH_SEMI_CONDENSED
 , stretchNormal         = PANGO_STRETCH_NORMAL
 , stretchSemiExpanded   = PANGO_STRETCH_SEMI_EXPANDED
 , stretchExpanded       = PANGO_STRETCH_EXPANDED
 , stretchExtraExpanded  = PANGO_STRETCH_EXTRA_EXPANDED
 , stretchUltraExpanded  = PANGO_STRETCH_ULTRA_EXPANDED
 }

foreign import ccall pango_font_description_set_stretch
    :: Ptr FontDescription -> Stretch -> IO ()

foreign import ccall pango_font_description_get_stretch
    :: Ptr FontDescription -> IO Stretch

foreign import ccall pango_font_description_set_size
    :: Ptr FontDescription -> CInt -> IO ()

foreign import ccall pango_font_description_get_size
    :: Ptr FontDescription -> IO CInt

foreign import ccall pango_font_description_set_absolute_size
    :: Ptr FontDescription -> CDouble -> IO ()

foreign import ccall pango_font_description_get_size_is_absolute
    :: Ptr FontDescription -> IO CBool

newtype Gravity = Gravity CInt
#{ enum Gravity, Gravity
 , gravitySouth = PANGO_GRAVITY_SOUTH
 , gravityEast  = PANGO_GRAVITY_EAST
 , gravityNorth = PANGO_GRAVITY_NORTH
 , gravityWest  = PANGO_GRAVITY_WEST
 , gravityAuto  = PANGO_GRAVITY_AUTO
 }

foreign import ccall pango_font_description_set_gravity
    :: Ptr FontDescription -> Gravity -> IO ()

foreign import ccall pango_font_description_get_gravity
    :: Ptr FontDescription -> IO Gravity

newtype FontMask = FontMask CInt
#{ enum FontMask, FontMask
 , fontMaskFamily  = PANGO_FONT_MASK_FAMILY
 , fontMaskStyle   = PANGO_FONT_MASK_STYLE
 , fontMaskVariant = PANGO_FONT_MASK_VARIANT
 , fontMaskWeight  = PANGO_FONT_MASK_WEIGHT
 , fontMaskStretch = PANGO_FONT_MASK_STRETCH
 , fontMaskSize    = PANGO_FONT_MASK_SIZE
 , fontMaskGravity = PANGO_FONT_MASK_GRAVITY
 }

foreign import ccall pango_font_description_get_set_fields
    :: Ptr FontDescription -> IO FontMask

foreign import ccall pango_font_description_unset_fields
    :: Ptr FontDescription -> FontMask -> IO ()

foreign import ccall pango_font_description_merge
    :: Ptr FontDescription -> Ptr FontDescription -> CBool -> IO ()

foreign import ccall pango_font_description_better_match
    :: Ptr FontDescription -> Ptr FontDescription -> Ptr FontDescription -> IO CBool

foreign import ccall pango_font_description_from_string
    :: CString -> IO (Ptr FontDescription)

foreign import ccall pango_font_description_to_string
    :: Ptr FontDescription -> IO CString

foreign import ccall pango_font_description_to_filename
    :: Ptr FontDescription -> IO CString

newtype FontMap    = FontMap    (ForeignPtr FontMap)
newtype FontFamily = FontFamily (ForeignPtr FontFamily)
newtype FontFace   = FontFace   (ForeignPtr FontFace)

foreign import ccall pango_font_family_get_name
    :: Ptr FontFamily -> IO CString

foreign import ccall pango_font_family_is_monospace
    :: Ptr FontFamily -> IO CBool

foreign import ccall pango_font_family_list_faces
    :: Ptr FontFamily -> Ptr (Ptr (Ptr FontFace)) -> Ptr CInt -> IO ()

foreign import ccall pango_font_face_get_face_name
    :: Ptr FontFace -> IO CString

foreign import ccall pango_font_face_list_sizes
    :: Ptr FontFace -> Ptr (Ptr CInt) -> Ptr CInt -> IO ()

foreign import ccall pango_font_face_describe
    :: Ptr FontFace -> IO (Ptr FontDescription)

foreign import ccall pango_font_face_is_synthesized
    :: Ptr FontFace -> IO CBool

foreign import ccall pango_font_map_list_families
    :: Ptr FontMap -> Ptr (Ptr (Ptr FontFamily)) -> Ptr CInt -> IO ()

