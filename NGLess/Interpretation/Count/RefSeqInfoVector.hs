{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{- Copyright 2013-2018 NGLess Authors
 - License: MIT
 -}

module Interpretation.Count.RefSeqInfoVector
    ( RefSeqInfo(..)
    , RefSeqInfoVectorMutable
    , RefSeqInfoVector
    , fromList
    , newRefSeqInfoVector
    , insert
    , sort
    , unsafeFreeze
    , unsafeThaw
    , lookup
    , retrieveSize
    , retrieveSizeIO
    , retrieveName
    , length
    , writeSizeIO
    ) where

import           Prelude hiding (length, lookup)
import           Data.Monoid ((<>))
import           Foreign.C.Types
import           Control.DeepSeq (NFData(..))
import           Control.Monad (forM_)
import qualified Data.ByteString as B


import           Foreign.Ptr
import           Foreign.ForeignPtr
import           System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Unsafe as CU
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C


foreign import ccall "&rsiv_free" c_rsiv_free :: FunPtr (Ptr () -> IO ())

C.context (C.baseCtx <> C.bsCtx <> C.fptrCtx <> C.cppCtx)
C.include "RefSeqInfoVector.h"

data RefSeqInfo = RefSeqInfo
                        { rsiName :: {-# UNPACK #-} !B.ByteString
                        , rsiSize :: {-# UNPACK #-} !Double
                        } deriving (Eq, Show)
instance NFData RefSeqInfo where
    rnf !_ = ()

instance Ord RefSeqInfo where
    compare RefSeqInfo{ rsiName = n0 } RefSeqInfo{ rsiName = n1 } = compare n0 n1


newtype RefSeqInfoVectorMutable = RefSeqInfoVectorMutable (ForeignPtr ())
newtype RefSeqInfoVector = RefSeqInfoVector (ForeignPtr ())

instance NFData RefSeqInfoVector where
    rnf (RefSeqInfoVector !_) = ()

newRefSeqInfoVector :: IO RefSeqInfoVectorMutable
newRefSeqInfoVector = do
    p <- C.withPtr_ $ \r ->
        [C.catchBlock| { *( $(void** r) ) = new RefSeqInfoVector; }|]
    RefSeqInfoVectorMutable <$> newForeignPtr c_rsiv_free (p :: Ptr ())

insert :: RefSeqInfoVectorMutable -> B.ByteString -> Double -> IO ()
insert (RefSeqInfoVectorMutable p) bs val = do
        let val' :: CDouble
            val' = CDouble val
        [C.catchBlock| { static_cast<RefSeqInfoVector*>($fptr-ptr:(void* p))->insert(std::string($bs-ptr:bs, $bs-len:bs), $(double val')); } |]

sort :: RefSeqInfoVectorMutable -> IO ()
sort (RefSeqInfoVectorMutable p) = [CU.block| void {
            RefSeqInfoVector* vec = static_cast<RefSeqInfoVector*>($fptr-ptr:(void* p));
            vec->sort();
    }|]

unsafeFreeze :: RefSeqInfoVectorMutable -> IO RefSeqInfoVector
unsafeFreeze (RefSeqInfoVectorMutable v) = return $ RefSeqInfoVector v

unsafeThaw :: RefSeqInfoVector -> IO RefSeqInfoVectorMutable
unsafeThaw (RefSeqInfoVector v) = return $ RefSeqInfoVectorMutable v

lookup :: RefSeqInfoVector -> B.ByteString -> Maybe Int
lookup (RefSeqInfoVector p) key = let
    CInt ix = [CU.pure| int { static_cast<RefSeqInfoVector*>($fptr-ptr:(void* p))->find(std::string($bs-ptr:key, $bs-len:key).c_str()) } |]
    in if ix == -1
        then Nothing
        else Just (fromEnum ix)

retrieveSize :: RefSeqInfoVector -> Int -> Double
retrieveSize (RefSeqInfoVector r) ix = unsafeDupablePerformIO (retrieveSizeIO (RefSeqInfoVectorMutable r) ix)

retrieveSizeIO :: RefSeqInfoVectorMutable -> Int -> IO Double
retrieveSizeIO (RefSeqInfoVectorMutable p) ix = do
    let ix' = toEnum ix
    CDouble val <- [CU.exp| double { static_cast<RefSeqInfoVector*>($fptr-ptr:(void* p))->at($(int ix')).val } |]
    return val

writeSizeIO :: RefSeqInfoVectorMutable -> Int -> Double -> IO ()
writeSizeIO (RefSeqInfoVectorMutable p) ix val = do
    let val' = CDouble val
        ix' = toEnum ix
    [C.catchBlock| { static_cast<RefSeqInfoVector*>($fptr-ptr:(void* p))->at($(int ix')).val = $(double val'); } |]

length :: RefSeqInfoVector -> Int
length (RefSeqInfoVector p) =
    fromEnum [CU.pure| unsigned int { static_cast<RefSeqInfoVector*>($fptr-ptr:(void* p))->size() } |]

retrieveName :: RefSeqInfoVector -> Int -> B.ByteString
retrieveName (RefSeqInfoVector p) ix = unsafeDupablePerformIO $ do
    let ix' = toEnum ix
    [CU.exp| const char* {static_cast<RefSeqInfoVector*>($fptr-ptr:(void* p))->at($(int ix')).str } |] >>= B.packCString

fromList :: [RefSeqInfo] -> RefSeqInfoVector
fromList entries = unsafeDupablePerformIO $ do
    r <- newRefSeqInfoVector
    forM_ entries $ \(RefSeqInfo n v) -> insert r n v
    sort r
    unsafeFreeze r
