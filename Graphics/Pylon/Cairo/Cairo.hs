{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Graphics.Pylon.Cairo.Cairo where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Graphics.Pylon.Foreign.Cairo.Cairo
import Graphics.Pylon.Binding.Cairo.Cairo as C

import Graphics.Pylon.Foreign.Cairo.Surface

import Graphics.Pylon.Types

import Data.Reflection

data CairoW = forall s. CairoW (Cairo s)

newtype Render s a = Render { unRender :: ReaderT (Cairo s) IO a}
    deriving (Functor, Applicative, Monad, MonadIO)

renderWith :: Surface s -> (Given CairoW => Render s a) -> IO a
renderWith s m = withCairo s $ \c ->
    give (CairoW c) $ runReaderT (unRender m) c

liftRender :: (Cairo s -> IO a) -> Render s a
liftRender f = do
    cxt <- Render ask
    liftIO $ f cxt

setSourceColor :: Color Double -> Render s ()
setSourceColor c = do
    cxt <- Render ask
    liftIO $ case c of
        RGB  r g b   -> setSourceRgb  cxt r g b
        RGBA r g b a -> setSourceRgba cxt r g b a

fill :: Render s ()
fill = liftRender C.fill
