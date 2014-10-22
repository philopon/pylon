module Graphics.Pylon.Binding.Pango.Font where

import Graphics.Pylon.Foreign.Pango.Font
import Graphics.Pylon.Foreign.GLib
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as S
import Foreign.Marshal
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String

fontDescriptionNew :: IO FontDescription
fontDescriptionNew = fmap FontDescription $
    pango_font_description_new >>=
        newForeignPtr p'pango_font_description_free

fontDescriptionCopy :: FontDescription -> IO FontDescription
fontDescriptionCopy (FontDescription f'fd) = fmap FontDescription $
    withForeignPtr f'fd $ \fd ->
    pango_font_description_copy fd >>=
        newForeignPtr p'pango_font_description_free

fontDescriptionEqual :: FontDescription -> FontDescription -> IO Bool
fontDescriptionEqual (FontDescription f'a) (FontDescription f'b) =
    withForeignPtr f'a $ \a -> withForeignPtr f'b $ \b -> 
    fmap toBool $ pango_font_description_equal a b

fontDescriptionSetFamily :: S.ByteString -> FontDescription -> IO ()
fontDescriptionSetFamily fam (FontDescription f'fd) =
    S.useAsCString fam $ \f -> withForeignPtr f'fd $ \fd ->
    pango_font_description_set_family fd f

fontDescriptionGetFamily :: FontDescription -> IO (Maybe S.ByteString)
fontDescriptionGetFamily (FontDescription f'fd) =
    withForeignPtr f'fd $ \fd -> do
        p <- pango_font_description_get_family fd
        if p == nullPtr
            then return Nothing
            else Just `fmap` S.unsafePackCString p

fontDescriptionSetStyle :: Style -> FontDescription -> IO ()
fontDescriptionSetStyle sty (FontDescription f'fd) =
    withForeignPtr f'fd $ \fd -> pango_font_description_set_style fd sty

fontDescriptionGetStyle :: FontDescription -> IO Style
fontDescriptionGetStyle (FontDescription f'fd) =
    withForeignPtr f'fd $ \fd ->
    pango_font_description_get_style fd

fontDescriptionSetVariant :: Variant -> FontDescription -> IO ()
fontDescriptionSetVariant var (FontDescription f'fd) =
    withForeignPtr f'fd $ \fd ->
    pango_font_description_set_variant fd var

fontDescriptionGetVariant :: FontDescription -> IO Variant
fontDescriptionGetVariant (FontDescription f'fd) =
    withForeignPtr f'fd $ \fd ->
    pango_font_description_get_variant fd

fontDescriptionSetWeight :: Weight -> FontDescription -> IO ()
fontDescriptionSetWeight w (FontDescription f'fd) =
    withForeignPtr f'fd $ \fd ->
    pango_font_description_set_weight fd w

fontDescriptionGetWeight :: FontDescription -> IO Weight
fontDescriptionGetWeight (FontDescription f'fd) =
    withForeignPtr f'fd $ \fd ->
    pango_font_description_get_weight fd

fontDescriptionSetStretch :: Stretch -> FontDescription -> IO ()
fontDescriptionSetStretch s (FontDescription f'fd) =
    withForeignPtr f'fd $ \fd ->
    pango_font_description_set_stretch fd s

fontDescriptionGetStretch :: FontDescription -> IO Stretch
fontDescriptionGetStretch (FontDescription f'fd) =
    withForeignPtr f'fd $ \fd ->
    pango_font_description_get_stretch fd

fontDescriptionSetSize :: Int -> FontDescription -> IO ()
fontDescriptionSetSize s (FontDescription f'fd) =
    withForeignPtr f'fd $ \fd ->
    pango_font_description_set_size fd (fromIntegral s)

fontDescriptionGetSize :: FontDescription -> IO Int
fontDescriptionGetSize (FontDescription f'fd) =
    withForeignPtr f'fd $ \fd ->
    fromIntegral `fmap` pango_font_description_get_size fd

fontDescriptionSetAbsoluteSize :: Int -> FontDescription -> IO ()
fontDescriptionSetAbsoluteSize s (FontDescription f'fd) =
    withForeignPtr f'fd $ \fd ->
    pango_font_description_set_absolute_size fd (fromIntegral s)

fontDescriptionGetSizeIsAbsolute :: FontDescription -> IO Bool
fontDescriptionGetSizeIsAbsolute (FontDescription f'fd) =
    withForeignPtr f'fd $ \fd ->
    toBool `fmap` pango_font_description_get_size_is_absolute fd

fontDescriptionSetGravity :: Gravity -> FontDescription -> IO ()
fontDescriptionSetGravity g (FontDescription f'fd) =
    withForeignPtr f'fd $ \fd ->
    pango_font_description_set_gravity fd g

fontDescriptionGetGravity :: FontDescription -> IO Gravity
fontDescriptionGetGravity (FontDescription f'fd) =
    withForeignPtr f'fd $ \fd ->
    pango_font_description_get_gravity fd

fontDescriptionGetSetFields :: FontDescription -> IO FontMask
fontDescriptionGetSetFields (FontDescription f'fd) =
    withForeignPtr f'fd $ \fd ->
    pango_font_description_get_set_fields fd

fontDescriptionUnsetFields :: FontMask -> FontDescription -> IO ()
fontDescriptionUnsetFields mask (FontDescription f'fd) =
    withForeignPtr f'fd $ \fd ->
    pango_font_description_unset_fields fd mask

fontDescriptionMerge :: FontDescription -> FontDescription -> Bool -> IO ()
fontDescriptionMerge (FontDescription f'to) (FontDescription f'from) rep =
    withForeignPtr f'to $ \to -> withForeignPtr f'from $ \from ->
    pango_font_description_merge to from (fromBool rep)

fontDescriptionFromString :: S.ByteString -> IO FontDescription
fontDescriptionFromString s =
    S.unsafeUseAsCString s $ \str ->
    pango_font_description_from_string str >>= \p ->
    fmap FontDescription $
        newForeignPtr p'pango_font_description_free p

fontDescriptionToString :: FontDescription -> IO S.ByteString
fontDescriptionToString (FontDescription f'fd) =
    withForeignPtr f'fd $ \fd ->
    pango_font_description_to_string fd >>= \p -> do
        r <- S.packCString p
        g_free p
        return r

fontDescriptionToFilename :: FontDescription -> IO FilePath
fontDescriptionToFilename (FontDescription f'fd) =
    withForeignPtr f'fd $ \fd ->
    pango_font_description_to_filename fd >>= \p -> do
        f <- peekCString p
        g_free p
        return f

fontMapListFamilies :: FontMap -> IO [FontFamily]
fontMapListFamilies (FontMap f'fm) =
    withForeignPtr f'fm $ \fm ->
    alloca $ \pfams -> alloca $ \nfam -> do
        pango_font_map_list_families fm pfams nfam
        n    <- peek nfam
        fams <- peek pfams
        fs   <- peekArray (fromIntegral n) fams
        g_free fams
        mapM (\p -> FontFamily `fmap` newForeignPtr_ p) fs

fontFamilyGetName :: FontFamily -> IO S.ByteString
fontFamilyGetName (FontFamily fam) = withForeignPtr fam $ \p ->
    pango_font_family_get_name p >>= S.unsafePackCString

fontFamilyIsMonospace :: FontFamily -> IO Bool
fontFamilyIsMonospace (FontFamily fam) = withForeignPtr fam $ \p ->
    toBool `fmap` pango_font_family_is_monospace p

fontFamilyListFaces :: FontFamily -> IO [FontFace]
fontFamilyListFaces (FontFamily f'fam) = withForeignPtr f'fam $ \fam ->
    alloca $ \pfaces -> alloca $ \pnfaces -> do
        pango_font_family_list_faces fam pfaces pnfaces
        nfaces <- peek pnfaces
        faces  <- peek pfaces
        fs     <- peekArray (fromIntegral nfaces) faces
        g_free faces
        mapM (\p -> FontFace `fmap` newForeignPtr_ p) fs

fontFaceGetFaceName :: FontFace -> IO S.ByteString
fontFaceGetFaceName (FontFace f) = withForeignPtr f $ \p ->
    pango_font_face_get_face_name p >>= S.unsafePackCString

fontFaceListSizes :: FontFace -> IO [Int]
fontFaceListSizes (FontFace f'face) = withForeignPtr f'face $ \face ->
    alloca $ \psizes -> alloca $ \pnsizes -> do
        pango_font_face_list_sizes face psizes pnsizes
        nsizes <- peek pnsizes
        sizes  <- peek psizes
        ss     <- peekArray (fromIntegral nsizes) sizes
        g_free sizes
        return $ map fromIntegral ss

fontFaceDescribe :: FontFace -> IO FontDescription
fontFaceDescribe (FontFace f) = withForeignPtr f $ \p ->
    pango_font_face_describe p >>= newForeignPtr_ >>= return . FontDescription

fontFaceIsSynthesized :: FontFace -> IO Bool
fontFaceIsSynthesized (FontFace f) = withForeignPtr f $ \p ->
    toBool `fmap` pango_font_face_is_synthesized p
