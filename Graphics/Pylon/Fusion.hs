module Graphics.Pylon.Fusion where

import System.IO.Unsafe

data Stream a = Stream (a -> IO a) a

class Copy a where
    copy :: a -> IO a

stream :: Copy a => a -> Stream a
stream = Stream copy
{-# INLINE [0] stream #-}

unstream :: Stream a -> a
unstream (Stream f i) = unsafePerformIO $ f i
{-# INLINE [0] unstream #-}

{-# RULES
 "stream/unstream" [1] forall s. stream (unstream s) = s
 #-}

streamSetter :: Copy a => (a -> IO ()) -> Stream a -> Stream a
streamSetter g (Stream f i) = Stream go i
  where
    go j = f j >>= \r -> g r >> return r
{-# INLINE [0] streamSetter #-}

streamGetter :: (a -> IO b) -> Stream a -> b
streamGetter g (Stream f i) = unsafePerformIO $ f i >>= g
{-# INLINE [0] streamGetter #-}
