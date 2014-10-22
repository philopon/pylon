module Graphics.Pylon.Binding.Pango.Layout where

import Graphics.Pylon.Foreign.Pango.Layout
import Graphics.Pylon.Foreign.Pango.Font
import Graphics.Pylon.Foreign.GObject

import qualified Data.Text as T
import qualified Data.Text.Foreign as T

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Utils

layoutCopy :: Layout -> IO Layout
layoutCopy (Layout f'lay) = withForeignPtr f'lay $ \lay -> do
    n <- pango_layout_copy lay
    Layout `fmap` newForeignPtr p'g_object_unref n

contextChanged :: Layout -> IO ()
contextChanged (Layout f'lay) = withForeignPtr f'lay $ \lay ->
    pango_layout_context_changed lay

setText :: T.Text -> Layout -> IO ()
setText txt (Layout f'lay) =
    withForeignPtr f'lay $ \lay ->
    T.withCStringLen txt $ \(str, len) ->
    pango_layout_set_text lay str (fromIntegral len)

getText :: Layout -> IO T.Text
getText (Layout f'lay) =
    withForeignPtr f'lay $ \lay -> do
        n <- pango_layout_get_character_count lay
        t <- pango_layout_get_text lay
        T.peekCStringLen (t, fromIntegral n)

setMarkup :: T.Text -> Layout -> IO ()
setMarkup txt (Layout f'lay) =
    withForeignPtr f'lay $ \lay ->
    T.withCStringLen txt $ \(str, len) ->
    pango_layout_set_markup lay str (fromIntegral len)

setFontDescription :: FontDescription -> Layout -> IO ()
setFontDescription (FontDescription f'fd) (Layout f'lay) =
    withForeignPtr f'lay $ \lay ->
    withForeignPtr f'fd  $ \fd  ->
    pango_layout_set_font_description lay fd

unsetFontDescription :: Layout -> IO ()
unsetFontDescription (Layout f'lay) =
    withForeignPtr f'lay $ \lay ->
    pango_layout_set_font_description lay nullPtr

getFontDescription :: Layout -> IO FontDescription
getFontDescription (Layout f'lay) = withForeignPtr f'lay $ \lay -> do
    fd <- pango_layout_get_font_description lay
    FontDescription `fmap` newForeignPtr_ fd

setWidth :: Int -> Layout -> IO ()
setWidth w (Layout f) = withForeignPtr f $ \l ->
    pango_layout_set_width l (fromIntegral w)

getWidth :: Layout -> IO Int
getWidth (Layout f) = withForeignPtr f $ \l ->
    fromIntegral `fmap` pango_layout_get_width l

setHeight :: Int -> Layout -> IO ()
setHeight h (Layout f) = withForeignPtr f $ \l ->
    pango_layout_set_height l (fromIntegral h)

getHeight :: Layout -> IO Int
getHeight (Layout f) = withForeignPtr f $ \l ->
    fromIntegral `fmap` pango_layout_get_height l

setWrap :: WrapMode -> Layout -> IO ()
setWrap w (Layout f) = withForeignPtr f $ \l ->
    pango_layout_set_wrap l w

getWrap :: Layout -> IO WrapMode
getWrap (Layout f) = withForeignPtr f $
    pango_layout_get_wrap

isWrapped :: Layout -> IO Bool
isWrapped (Layout f) = withForeignPtr f $ \l ->
    toBool `fmap` pango_layout_is_wrapped l

setEllipsize :: EllipsizeMode -> Layout -> IO ()
setEllipsize e (Layout f) = withForeignPtr f $ \l ->
    pango_layout_set_ellipsize l e

getEllipsize :: Layout -> IO EllipsizeMode
getEllipsize (Layout f) = withForeignPtr f $
    pango_layout_get_ellipsize

isEllipsized :: Layout -> IO Bool
isEllipsized (Layout f) = withForeignPtr f $ \l ->
    toBool `fmap` pango_layout_is_ellipsized l

setIndent :: Int -> Layout -> IO ()
setIndent i (Layout f) = withForeignPtr f $ \l ->
    pango_layout_set_indent l (fromIntegral i)

getIndent :: Layout -> IO Int
getIndent (Layout f) = withForeignPtr f $ \l ->
    fromIntegral `fmap` pango_layout_get_indent l

setSpacing :: Int -> Layout -> IO ()
setSpacing i (Layout f) = withForeignPtr f $ \l ->
    pango_layout_set_spacing l (fromIntegral i)

getSpacing :: Layout -> IO Int
getSpacing (Layout f) = withForeignPtr f $ \l ->
    fromIntegral `fmap` pango_layout_get_spacing l

setJustify :: Bool -> Layout -> IO ()
setJustify j (Layout f) = withForeignPtr f $ \l ->
    pango_layout_set_justify l (fromBool j)

getJustify :: Layout -> IO Bool
getJustify (Layout f) = withForeignPtr f $ \l ->
    toBool `fmap` pango_layout_get_justify l

setAutoDir :: Bool -> Layout -> IO ()
setAutoDir a (Layout f) = withForeignPtr f $ \l ->
    pango_layout_set_auto_dir l (fromBool a)

getAutoDir :: Layout -> IO Bool
getAutoDir (Layout f) = withForeignPtr f $ \l ->
    toBool `fmap` pango_layout_get_auto_dir l

setAlignment :: Alignment -> Layout -> IO ()
setAlignment a (Layout f) = withForeignPtr f $ \l ->
    pango_layout_set_alignment l a

getAlignment :: Layout -> IO Alignment
getAlignment (Layout f) = withForeignPtr f $
    pango_layout_get_alignment

setSingleParagraphMode :: Bool -> Layout -> IO ()
setSingleParagraphMode a (Layout f) = withForeignPtr f $ \l ->
    pango_layout_set_single_paragraph_mode l (fromBool a)

getSingleParagraphMode :: Layout -> IO Bool
getSingleParagraphMode (Layout f) = withForeignPtr f $ \l ->
    toBool `fmap` pango_layout_get_single_paragraph_mode l

getUnknownGryphsCount :: Layout -> IO Int
getUnknownGryphsCount (Layout f) = withForeignPtr f $ \l ->
    fromIntegral `fmap` pango_layout_get_unknown_glyphs_count l
