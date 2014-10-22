{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Pylon.Pango.Layout where

import qualified Graphics.Pylon.Foreign.Pango.Layout as F
import Graphics.Pylon.Binding.Pango.Layout as F
import Graphics.Pylon.Binding.Pango.Cairo  as F
import Graphics.Pylon.Fusion
import Graphics.Pylon.Types
import Graphics.Pylon.Common

import Graphics.Pylon.Cairo.Cairo
import Graphics.Pylon.Cairo.Path
import Graphics.Pylon.Pango.Font

import qualified Data.Text as T
import Data.Default.Class
import Data.Reflection

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import System.IO.Unsafe
import Data.String


newtype Layout = Layout F.Layout

instance Copy Layout where
    copy (Layout fd) = Layout `fmap` layoutCopy fd

instance Given CairoW => Default Layout where
    def = case given of
        CairoW c -> Layout . unsafePerformIO $ createLayout c
    {-# NOINLINE def #-}

instance Given CairoW => IsString Layout where
    fromString s = case given of
        CairoW c -> Layout . unsafePerformIO $ do
            l <- createLayout c
            setMarkup (T.pack s) l
            return l

instance Drawable Layout where
    draw (Layout a) = do
        cxt <- Render ask
        liftIO $ updateLayout cxt a >> showLayout cxt a

mkLLens :: (F.Layout -> IO b) -> (b -> F.Layout -> IO ()) -> Lens' Layout b
mkLLens getter setter = mkLens getter' setter'
  where
    getter'   (Layout fd) = getter fd
    setter' b (Layout fd) = setter b fd
{-# INLINE mkLLens #-}

instance HasText Layout where
    text = mkLLens getText setText
    {-# INLINE text #-}

markup :: Setter' Layout T.Text
markup = mkLLens (const $ return T.empty) setMarkup
{-# INLINE markup #-}

fontDescription :: Lens' Layout (Maybe FontDescription)
fontDescription = mkLLens getter setter
  where
    getter l = (Just . FontDescription) `fmap` getFontDescription l
    setter (Just (FontDescription fd)) = setFontDescription fd
    setter Nothing = unsetFontDescription
{-# INLINE fontDescription #-}

instance HasWidth Layout Int where
    width = mkLLens getWidth setWidth
    {-# INLINE width #-}

instance HasHeight Layout Int where
    height = mkLLens getHeight setHeight
    {-# INLINE height #-}

wrap :: Lens' Layout F.WrapMode
wrap = mkLLens getWrap setWrap
{-# INLINE wrap #-}

isWrapped :: Getter Layout Bool
isWrapped = mkLLens F.isWrapped (\_ _ -> return ())
{-# INLINE isWrapped #-}

ellipsize :: Lens' Layout F.EllipsizeMode
ellipsize = mkLLens getEllipsize setEllipsize
{-# INLINE ellipsize #-}

isEllipsized :: Getter Layout Bool
isEllipsized = mkLLens F.isEllipsized (\_ _ -> return ())
{-# INLINE isEllipsized #-}

indent :: Lens' Layout Int
indent = mkLLens getIndent setIndent
{-# INLINE indent #-}

spacing :: Lens' Layout Int
spacing = mkLLens getSpacing setSpacing
{-# INLINE spacing #-}

justify :: Lens' Layout Bool
justify = mkLLens getJustify setJustify
{-# INLINE justify #-}

autoDir :: Lens' Layout Bool
autoDir = mkLLens getAutoDir setAutoDir
{-# INLINE autoDir #-}

alignment :: Lens' Layout F.Alignment
alignment = mkLLens getAlignment setAlignment
{-# INLINE alignment #-}

singleParagraphMode :: Lens' Layout Bool
singleParagraphMode = mkLLens getSingleParagraphMode setSingleParagraphMode
{-# INLINE singleParagraphMode #-}

unknownGryphsCount :: Getter Layout Int
unknownGryphsCount = mkLLens getUnknownGryphsCount (\_ _ -> return ())


