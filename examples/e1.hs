{-# LANGUAGE OverloadedStrings #-}
import Graphics.Pylon.Foreign.Cairo.Types

import Graphics.Pylon.Cairo.Surface
import Graphics.Pylon.Cairo.Cairo
import Graphics.Pylon.Cairo.Path

import Graphics.Pylon.Pango.Cairo
import Graphics.Pylon.Pango.Layout
import Graphics.Pylon.Pango.Font

main = withSVGFile "e1.svg" 300 100 $ \surf -> withCairo surf $ \cairo -> do
    setSourceRgb cairo 1 0 0
    rectangle cairo 10 10 50 50
    fill cairo

    lay <- createLayout cairo
    setText lay "Hello, world!"
    fd  <- fontDescriptionFromString "Sans Bold 12"
    setFontDescription lay fd

    setSourceRgb cairo 0 0 1
    -- updateLayout cairo lay
    showLayout cairo lay

