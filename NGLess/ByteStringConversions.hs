
module ByteStringConversions
    (
    	toStrict
    ) where

import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.ByteString.Internal as BI

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B

import Foreign.ForeignPtr
import Foreign.Ptr


toStrict :: BL.ByteString -> B.ByteString
toStrict BLI.Empty = B.empty
toStrict (BLI.Chunk c BLI.Empty) = c
toStrict lb = BI.unsafeCreate len $ go lb
  where
    len = BLI.foldlChunks (\l sb -> l + B.length sb) 0 lb

    go  BLI.Empty                   _   = return ()
    go (BLI.Chunk (BI.PS fp s l) r) ptr =
        withForeignPtr fp $ \p -> do
            BI.memcpy ptr (p `plusPtr` s) (fromIntegral l)
            go r (ptr `plusPtr` l)