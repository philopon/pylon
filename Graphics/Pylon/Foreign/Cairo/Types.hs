{-# LANGUAGE Rank2Types #-}
module Graphics.Pylon.Foreign.Cairo.Types where

import Foreign.C
import Foreign.Ptr
import Graphics.Pylon.Foreign.Cairo.Error

type CBool = CInt

data Device
data FontOptions
data Rectangle

data UserDataKey
type UserDataSetter d = forall a. Ptr d -> Ptr UserDataKey -> Ptr a -> FunPtr (Ptr a -> IO ()) -> IO CairoStatus
type UserDataGetter d = forall a. Ptr d -> Ptr UserDataKey -> IO (Ptr a)

type ReadFunc  a = Ptr a -> Ptr CUChar -> CUInt -> IO CairoStatus
type WriteFunc a = Ptr a -> Ptr CUChar -> CUInt -> IO CairoStatus
