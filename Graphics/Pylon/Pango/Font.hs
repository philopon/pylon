module Graphics.Pylon.Pango.Font where

import Graphics.Pylon.Foreign.Pango.Font
import qualified Data.ByteString as S
import qualified Data.Text.Encoding as T
import Foreign.ForeignPtr

fontDescriptionFromString s = fmap FontDescription $
    S.useAsCString (T.encodeUtf8 s) $ \str ->
    pango_font_description_from_string str >>=
    newForeignPtr p'pango_font_description_free
