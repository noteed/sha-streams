module System.IO.Streams.SHA where

import Data.Binary.Get
import System.IO.Streams.Internal (InputStream (..))
import qualified System.IO.Streams as S

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Data.Digest.Pure.SHA

sha1Input :: InputStream ByteString -> IO (InputStream ByteString, IO (Digest SHA1State))
sha1Input = shaInput sha1Incremental completeSha1Incremental

sha224Input :: InputStream ByteString -> IO (InputStream ByteString, IO (Digest SHA256State))
sha224Input = shaInput sha224Incremental completeSha224Incremental

sha256Input :: InputStream ByteString -> IO (InputStream ByteString, IO (Digest SHA256State))
sha256Input = shaInput sha256Incremental completeSha256Incremental

sha384Input :: InputStream ByteString -> IO (InputStream ByteString, IO (Digest SHA512State))
sha384Input = shaInput sha384Incremental completeSha384Incremental

sha512Input :: InputStream ByteString -> IO (InputStream ByteString, IO (Digest SHA512State))
sha512Input = shaInput sha512Incremental completeSha512Incremental

-- | Inspired by `S.countInput`. The returned IO action can be run only
-- when the input stream is exhausted, otherwise an error occurs.
shaInput :: Decoder a -> (Decoder a -> Int -> Digest a)
  -> InputStream ByteString -> IO (InputStream ByteString, IO (Digest a))
shaInput increment end is = do
  ref <- newIORef (increment, 0)
  is' <- S.makeInputStream $ prod ref
  return $! (is', readIORef ref >>= uncurry complete)

  where

  prod ref = do
    mbs <- S.read is
    maybe
      (return Nothing)
      (\bs -> (modifyRef ref (uncurry $ modify bs)) >> (return $! Just bs))
      mbs

  complete decoder c = return $! end decoder c
  modify bs decoder c = (pushChunk decoder bs, c + (fromIntegral $ C.length bs))

-- | Taken from System.IO.Streams.ByteString.
{-# INLINE modifyRef #-}
modifyRef :: IORef a -> (a -> a) -> IO ()
modifyRef ref f = do
    x <- readIORef ref
    writeIORef ref $! f x
