{- Copyright 2015-2018 NGLess Authors
 - License: MIT
 -}
module Dependencies.Embedded
    ( samtoolsData
    , bwaData
    , prodigalData
    , megahitData
    , minimap2Data
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

foreign import ccall safe "get_prodigal_len" c_get_prodigal_len :: CUInt
foreign import ccall safe "get_prodigal_data" c_get_prodigal_data :: CString

foreign import ccall safe "get_megahit_len" c_get_megahit_len :: CUInt
foreign import ccall safe "get_megahit_data" c_get_megahit_data :: CString

foreign import ccall safe "get_minimap2_len" c_get_minimap2_len :: CUInt
foreign import ccall safe "get_minimap2_data" c_get_minimap2_data :: CString

samtoolsData :: IO B.ByteString
samtoolsData =
    B.unsafePackCStringLen (c_get_samtools_data, convert c_get_samtools_len)

prodigalData :: IO B.ByteString
prodigalData =
    B.unsafePackCStringLen (c_get_prodigal_data, convert c_get_prodigal_len)

bwaData :: IO B.ByteString
bwaData =
    B.unsafePackCStringLen (c_get_bwa_data, convert c_get_bwa_len)

megahitData :: IO B.ByteString
megahitData =
    B.unsafePackCStringLen (c_get_megahit_data, convert c_get_megahit_len)

minimap2Data :: IO B.ByteString
minimap2Data =
    B.unsafePackCStringLen (c_get_minimap2_data, convert c_get_minimap2_len)


