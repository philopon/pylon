{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverlappingInstances #-}

import Graphics.Pylon.Foreign.Cairo.Types

import Graphics.Pylon.Binding.Cairo.Surface
import Graphics.Pylon.Binding.Cairo.Cairo
import Graphics.Pylon.Binding.Cairo.Path

import Graphics.Pylon.Binding.Pango.Cairo
import Graphics.Pylon.Binding.Pango.Layout
import Graphics.Pylon.Binding.Pango.Font
import Graphics.Pylon.Foreign.Pango.Font

import Graphics.Pylon.Pango.Font as P
import Graphics.Pylon.Pango.Layout as P
import Graphics.Pylon.Cairo.Cairo as P
import Graphics.Pylon.Cairo.Path as P
import Graphics.Pylon.Types as P
import Data.Default.Class
import Control.Lens

main = withSVGFile (Just "e1.svg") (118/1.33) (18/1.33) $ \surf ->
    renderWith surf $ do
        setSourceColor def
        draw $ P.rectangle
            & position . _x .~ (4 :: Double)
            & position . _y .~ 4
            & width  .~ 50
            & height .~ 50

        let fd = "Verdana Bold"
                & size   .~ (9 * 1024 * 72 `quot` 96)
                & weight .~ weightNormal

        setSourceColor $ def & red .~ 1
        draw $ "test text"
            & fontDescription .~ Just fd
