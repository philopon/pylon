{-# LANGUAGE Rank2Types #-}

module Graphics.Pylon.Common where

import Graphics.Pylon.Fusion
import Graphics.Pylon.Types

mkLens :: Copy a => (a -> IO b) -> (b -> a -> IO ()) -> Lens' a b
mkLens getter setter f fd = fmap (\n -> setter' n fd) (f $ getter' fd)
  where
    setter' n = unstream . (streamSetter $ setter n) . stream
    getter'   = streamGetter getter . stream
{-# INLINE mkLens #-}


