module Graphics.Pylon.Foreign.GLib where
import Foreign.Ptr

foreign import ccall g_free :: Ptr gpointer -> IO ()
