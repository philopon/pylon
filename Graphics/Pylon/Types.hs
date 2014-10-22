{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Graphics.Pylon.Types where

import Control.Applicative
import Data.Functor.Identity
import qualified Data.Text as T
import Data.Default.Class

type Lens  s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s   a   = Lens s s a a

type Setter  s t a b = (a -> Identity b) -> s -> Identity t
type Setter' s   a   = Setter s s a a

type Getting r s a = (a -> Const r a) -> s -> Const r s
type Getter    s a = forall r. Getting r s a

class HasSize a b | a -> b where
    size :: Lens' a b

class HasText a where
    text :: Lens' a T.Text

data Point a = Point
    { pointX :: a
    , pointY :: a
    }
instance Num a => Default (Point a) where
    def = Point 0 0

_x :: Lens' (Point a) a
_x f (Point x y) = fmap (\x' -> Point x' y) (f x)

_y :: Lens' (Point a) a
_y f (Point x y) = fmap (\y' -> Point x y') (f y)

class HasPosition a b | a -> b where
    position :: Lens' a (Point b)

class HasWidth a b | a -> b where
    width :: Lens' a b

class HasHeight a b | a -> b where
    height :: Lens' a b

data Color a
    = RGB { _red   :: a
          , _green :: a
          , _blue  :: a
          }
    | RGBA { _red   :: a
           , _green :: a
           , _blue  :: a
           , _alpha :: a
           }

instance Num a => Default (Color a) where
    def = RGB 0 0 0

red :: Lens' (Color a) a
red f (RGB  r g b)   = fmap (\r' -> RGB  r' g b)   (f r)
red f (RGBA r g b a) = fmap (\r' -> RGBA r' g b a) (f r)

green :: Lens' (Color a) a
green f (RGB  r g b)   = fmap (\g' -> RGB  r g' b)   (f g)
green f (RGBA r g b a) = fmap (\g' -> RGBA r g' b a) (f g)

blue :: Lens' (Color a) a
blue f (RGB  r g b)   = fmap (\b' -> RGB  r g b')   (f b)
blue f (RGBA r g b a) = fmap (\b' -> RGBA r g b' a) (f b)

alphaSetter :: Color a -> Maybe a -> Color a
alphaSetter c Nothing  = RGB  (_red c) (_green c) (_blue c)
alphaSetter c (Just a) = RGBA (_red c) (_green c) (_blue c) a

alpha :: Lens' (Color a) (Maybe a)
alpha f c@(RGB  _ _ _)   = fmap (alphaSetter c) (f Nothing)
alpha f c@(RGBA _ _ _ a) = fmap (alphaSetter c) (f $ Just a)

data Rectangle a = Rectangle
    { rectanglePos    :: Point a
    , rectangleWidth  :: a
    , rectangleHeight :: a
    }

rectangle :: Num a => Rectangle a
rectangle = def

instance Num a => Default (Rectangle a) where
    def = Rectangle def 0 0

instance HasPosition (Rectangle a) a where
    position f (Rectangle p w h) = fmap (\p' -> Rectangle p' w h) (f p)

instance HasWidth (Rectangle a) a where
    width f (Rectangle p w h) = fmap (\w' -> Rectangle p w' h) (f w)

instance HasHeight (Rectangle a) a where
    height f (Rectangle p w h) = fmap (\h' -> Rectangle p w h') (f h)
