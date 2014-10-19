{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
module Graphics.Pylon.Foreign.Cairo.Error where

#include <cairo.h>

import Control.Exception
import Data.Typeable
import Foreign.C.Types

newtype CairoStatus = CairoStatus {unCairoStatus :: CInt}
    deriving (Typeable, Eq)

instance Exception CairoStatus

#{ enum CairoStatus, CairoStatus
 , statusSuccess                  = CAIRO_STATUS_SUCCESS

 , statusNoMemory                 = CAIRO_STATUS_NO_MEMORY
 , statusInvalidRestore           = CAIRO_STATUS_INVALID_RESTORE
 , statusInvalidPopGroup          = CAIRO_STATUS_INVALID_POP_GROUP
 , statusNoCurrentPoint           = CAIRO_STATUS_NO_CURRENT_POINT
 , statusInvalidMatrix            = CAIRO_STATUS_INVALID_MATRIX
 , statusInvalidStatus            = CAIRO_STATUS_INVALID_STATUS
 , statusNullPointer              = CAIRO_STATUS_NULL_POINTER
 , statusInvalidString            = CAIRO_STATUS_INVALID_STRING
 , statusInvalidPathData          = CAIRO_STATUS_INVALID_PATH_DATA
 , statusReadError                = CAIRO_STATUS_READ_ERROR
 , statusWriteError               = CAIRO_STATUS_WRITE_ERROR
 , statusSurfaceFinished          = CAIRO_STATUS_SURFACE_FINISHED
 , statusSurfaceTypeMismatch      = CAIRO_STATUS_SURFACE_TYPE_MISMATCH
 , statusPatternTypeMismatch      = CAIRO_STATUS_PATTERN_TYPE_MISMATCH
 , statusInvalidContent           = CAIRO_STATUS_INVALID_CONTENT
 , statusInvalidFormat            = CAIRO_STATUS_INVALID_FORMAT
 , statusInvalidVisual            = CAIRO_STATUS_INVALID_VISUAL
 , statusFileNotFound             = CAIRO_STATUS_FILE_NOT_FOUND
 , statusInvalidDash              = CAIRO_STATUS_INVALID_DASH
 , statusInvalidDscComment        = CAIRO_STATUS_INVALID_DSC_COMMENT
 , statusInvalidIndex             = CAIRO_STATUS_INVALID_INDEX
 , statusClipNotRepresentable     = CAIRO_STATUS_CLIP_NOT_REPRESENTABLE
 , statusTempFileError            = CAIRO_STATUS_TEMP_FILE_ERROR
 , statusInvalidStride            = CAIRO_STATUS_INVALID_STRIDE
 , statusFontTypeMismatch         = CAIRO_STATUS_FONT_TYPE_MISMATCH
 , statusUserFontImmutable        = CAIRO_STATUS_USER_FONT_IMMUTABLE
 , statusUserFontError            = CAIRO_STATUS_USER_FONT_ERROR
 , statusNegativeCount            = CAIRO_STATUS_NEGATIVE_COUNT
 , statusInvalidClusters          = CAIRO_STATUS_INVALID_CLUSTERS
 , statusInvalidSlant             = CAIRO_STATUS_INVALID_SLANT
 , statusInvalidWeight            = CAIRO_STATUS_INVALID_WEIGHT
 , statusInvalidSize              = CAIRO_STATUS_INVALID_SIZE
 , statusUserFontNotImplemented   = CAIRO_STATUS_USER_FONT_NOT_IMPLEMENTED
 , statusDeviceTypeMismatch       = CAIRO_STATUS_DEVICE_TYPE_MISMATCH
 , statusDeviceError              = CAIRO_STATUS_DEVICE_ERROR
 , statusInvalidMeshConstruction  = CAIRO_STATUS_INVALID_MESH_CONSTRUCTION
 , statusDeviceFinished           = CAIRO_STATUS_DEVICE_FINISHED
#if 0
 , statusJbig2GlobalMissing       = CAIRO_STATUS_JBIG2_GLOBAL_MISSING
#endif

 , statusLastStatus               = CAIRO_STATUS_LAST_STATUS
 }


instance Show CairoStatus where
    show s = "Cairo Status " ++ case () of
        _ | s == statusSuccess                 -> "SUCCESS"
          | s == statusNoMemory                -> "NO_MEMORY"
          | s == statusInvalidRestore          -> "INVALID_RESTORE"
          | s == statusInvalidPopGroup         -> "INVALID_POP_GROUP"
          | s == statusNoCurrentPoint          -> "NO_CURRENT_POINT"
          | s == statusInvalidMatrix           -> "INVALID_MATRIX"
          | s == statusInvalidStatus           -> "INVALID_STATUS"
          | s == statusNullPointer             -> "NULL_POINTER"
          | s == statusInvalidString           -> "INVALID_STRING"
          | s == statusInvalidPathData         -> "INVALID_PATH_DATA"
          | s == statusReadError               -> "READ_ERROR"
          | s == statusWriteError              -> "WRITE_ERROR"
          | s == statusSurfaceFinished         -> "SURFACE_FINISHED"
          | s == statusSurfaceTypeMismatch     -> "SURFACE_TYPE_MISMATCH"
          | s == statusPatternTypeMismatch     -> "PATTERN_TYPE_MISMATCH"
          | s == statusInvalidContent          -> "INVALID_CONTENT"
          | s == statusInvalidFormat           -> "INVALID_FORMAT"
          | s == statusInvalidVisual           -> "INVALID_VISUAL"
          | s == statusFileNotFound            -> "FILE_NOT_FOUND"
          | s == statusInvalidDash             -> "INVALID_DASH"
          | s == statusInvalidDscComment       -> "INVALID_DSC_COMMENT"
          | s == statusInvalidIndex            -> "INVALID_INDEX"
          | s == statusClipNotRepresentable    -> "CLIP_NOT_REPRESENTABLE"
          | s == statusTempFileError           -> "TEMP_FILE_ERROR"
          | s == statusInvalidStride           -> "INVALID_STRIDE"
          | s == statusFontTypeMismatch        -> "FONT_TYPE_MISMATCH"
          | s == statusUserFontImmutable       -> "USER_FONT_IMMUTABLE"
          | s == statusUserFontError           -> "USER_FONT_ERROR"
          | s == statusNegativeCount           -> "NEGATIVE_COUNT"
          | s == statusInvalidClusters         -> "INVALID_CLUSTERS"
          | s == statusInvalidSlant            -> "INVALID_SLANT"
          | s == statusInvalidWeight           -> "INVALID_WEIGHT"
          | s == statusInvalidSize             -> "INVALID_SIZE"
          | s == statusUserFontNotImplemented  -> "USER_FONT_NOT_IMPLEMENTED"
          | s == statusDeviceTypeMismatch      -> "DEVICE_TYPE_MISMATCH"
          | s == statusDeviceError             -> "DEVICE_ERROR"
          | s == statusInvalidMeshConstruction -> "INVALID_MESH_CONSTRUCTION"
          | s == statusDeviceFinished          -> "DEVICE_FINISHED"
          | s == statusLastStatus              -> "LAST_STATUS"
          | otherwise                          -> show $ unCairoStatus s
