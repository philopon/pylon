{-# LANGUAGE Rank2Types #-}
module Graphics.Pylon.Foreign.Cairo.Types where

import Foreign.C
import Foreign.Ptr

type CBool = CInt
newtype CairoStatus = CairoStatus CInt

data Point a = Point
    { pointX :: a
    , pointY :: a
    }

data Size a = Size
    { width  :: a
    , height :: a
    }

data Rectangle a = Rectangle
    { rectPoint :: Point a
    , rectSize  :: Size a
    }

data Color = Color
    { red   :: Double
    , green :: Double
    , blue  :: Double
    , alpha :: Maybe Double
    }

rgb :: Double -> Double -> Double -> Color
rgb  r g b   = Color r g b Nothing
rgba :: Double -> Double -> Double -> Double -> Color
rgba r g b a = Color r g b (Just a)

data Device
data Pattern
data FontOptions
data UserDataKey
type UserDataSetter d = forall a. Ptr d -> Ptr UserDataKey -> Ptr a -> FunPtr (Ptr a -> IO ()) -> IO CairoStatus
type UserDataGetter d = forall a. Ptr d -> Ptr UserDataKey -> IO (Ptr a)
