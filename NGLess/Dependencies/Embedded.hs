{- Copyright 2015-2016 NGLess Authors
 - License: MIT
 -}
module Dependencies.Embedded
    ( samtoolsData
    , bwaData
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

import Foreign.C.Types
import Foreign.C.String
import Data.Convertible

foreign import ccall safe "get_samtools_len" c_get_samtools_len :: CUInt
foreign import ccall safe "get_samtools_data" c_get_samtools_data :: CString

foreign import ccall safe "get_bwa_len" c_get_bwa_len :: CUInt
foreign import ccall safe "get_bwa_data" c_get_bwa_data :: CString

samtoolsData :: IO B.ByteString
samtoolsData =
    B.unsafePackCStringLen (c_get_samtools_data, convert c_get_samtools_len)

bwaData :: IO B.ByteString
bwaData =
    B.unsafePackCStringLen (c_get_bwa_data, convert c_get_bwa_len)

