{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Pylon.Pango.Font where

import qualified Graphics.Pylon.Foreign.Pango.Font as F
import Graphics.Pylon.Binding.Pango.Font
import Graphics.Pylon.Fusion
import Graphics.Pylon.Types
import Graphics.Pylon.Common

import Data.String
import Data.Default.Class
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8

import System.IO.Unsafe

newtype FontDescription = FontDescription F.FontDescription

instance Default FontDescription where
    def = FontDescription $ unsafePerformIO fontDescriptionNew
    {-# NOINLINE def #-}

instance Copy FontDescription where
    copy (FontDescription fd) = FontDescription `fmap` fontDescriptionCopy fd

instance IsString FontDescription where
    fromString = FontDescription . unsafePerformIO .
        fontDescriptionFromString . S8.pack

mkFdLens :: (F.FontDescription -> IO b) -> (b -> F.FontDescription -> IO ()) -> Lens' FontDescription b
mkFdLens getter setter = mkLens getter' setter'
  where
    getter'   (FontDescription fd) = getter fd
    setter' b (FontDescription fd) = setter b fd
{-# INLINE mkFdLens #-}

family :: Lens' FontDescription (Maybe S.ByteString)
family = mkFdLens fontDescriptionGetFamily setter
  where
    setter (Just s) = fontDescriptionSetFamily s
    setter Nothing  = fontDescriptionUnsetFields F.fontMaskFamily
{-# INLINE family #-}

style :: Lens' FontDescription F.Style
style = mkFdLens fontDescriptionGetStyle fontDescriptionSetStyle
{-# INLINE style #-}

variant :: Lens' FontDescription F.Variant
variant = mkFdLens fontDescriptionGetVariant fontDescriptionSetVariant
{-# INLINE variant #-}

weight :: Lens' FontDescription F.Weight
weight = mkFdLens fontDescriptionGetWeight fontDescriptionSetWeight
{-# INLINE weight #-}

stretch :: Lens' FontDescription F.Stretch
stretch = mkFdLens fontDescriptionGetStretch fontDescriptionSetStretch
{-# INLINE stretch #-}

instance HasSize FontDescription Int where
    size = mkFdLens fontDescriptionGetSize fontDescriptionSetSize
    {-# INLINE size #-}

gravity :: Lens' FontDescription F.Gravity
gravity = mkFdLens fontDescriptionGetGravity fontDescriptionSetGravity
{-# INLINE gravity #-}
