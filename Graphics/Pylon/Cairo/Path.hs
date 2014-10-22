{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Graphics.Pylon.Cairo.Path where

import Graphics.Pylon.Foreign.Cairo.Cairo
import Graphics.Pylon.Types
import Graphics.Pylon.Binding.Cairo.Path as P
import Graphics.Pylon.Cairo.Cairo

class HasPath a where
    pathFunction :: forall s. a -> Cairo s -> IO ()

instance HasPath (Rectangle Double) where
    pathFunction (Rectangle (Point x y) w h) c = P.rectangle c x y w h

path :: HasPath a => a -> Render s ()
path = liftRender . pathFunction

class Drawable a where
    draw :: forall s. a -> Render s ()

instance HasPath a => Drawable a where
    draw a = path a >> fill
