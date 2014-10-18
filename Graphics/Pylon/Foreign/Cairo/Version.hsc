{-# LANGUAGE OverloadedStrings #-}
module Graphics.Pylon.Foreign.Cairo.Version where

import Data.ByteString ( ByteString )
#include <cairo.h>

version, versionMajor, versionMinor, versionMicro :: Int
version = #{const CAIRO_VERSION}
versionMajor = #{const CAIRO_VERSION_MAJOR}
versionMinor = #{const CAIRO_VERSION_MINOR}
versionMicro = #{const CAIRO_VERSION_MICRO}

versionString :: ByteString
versionString = #{const_str cairo_version_string()}
