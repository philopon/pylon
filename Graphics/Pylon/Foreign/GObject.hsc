module Graphics.Pylon.Foreign.GObject where

import Foreign.Ptr

foreign import ccall g_object_unref
    :: Ptr object -> IO ()

foreign import ccall "&g_object_unref" p'g_object_unref
    :: FunPtr (Ptr object -> IO ())
