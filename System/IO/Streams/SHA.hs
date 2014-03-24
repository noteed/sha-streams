{-# LANGUAGE DeriveDataTypeable #-}
module System.IO.Streams.SHA where

import Control.Exception (Exception, throwIO)
import Data.Binary.Get
import Data.Typeable (Typeable)
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

checkedSha1Input :: Digest SHA1State -> InputStream ByteString -> IO (InputStream ByteString)
checkedSha1Input = checkedShaInput sha1Incremental completeSha1Incremental . showDigest

checkedSha224Input :: Digest SHA256State -> InputStream ByteString -> IO (InputStream ByteString)
checkedSha224Input = checkedShaInput sha224Incremental completeSha224Incremental . showDigest

checkedSha256Input :: Digest SHA256State -> InputStream ByteString -> IO (InputStream ByteString)
checkedSha256Input = checkedShaInput sha256Incremental completeSha256Incremental . showDigest

checkedSha384Input :: Digest SHA512State -> InputStream ByteString -> IO (InputStream ByteString)
checkedSha384Input = checkedShaInput sha384Incremental completeSha384Incremental . showDigest

checkedSha512Input :: Digest SHA512State -> InputStream ByteString -> IO (InputStream ByteString)
checkedSha512Input = checkedShaInput sha512Incremental completeSha512Incremental . showDigest

checkedSha1Input' :: String -> InputStream ByteString -> IO (InputStream ByteString)
checkedSha1Input' = checkedShaInput sha1Incremental completeSha1Incremental

checkedSha224Input' :: String -> InputStream ByteString -> IO (InputStream ByteString)
checkedSha224Input' = checkedShaInput sha224Incremental completeSha224Incremental

checkedSha256Input' :: String -> InputStream ByteString -> IO (InputStream ByteString)
checkedSha256Input' = checkedShaInput sha256Incremental completeSha256Incremental

checkedSha384Input' :: String -> InputStream ByteString -> IO (InputStream ByteString)
checkedSha384Input' = checkedShaInput sha384Incremental completeSha384Incremental

checkedSha512Input' :: String -> InputStream ByteString -> IO (InputStream ByteString)
checkedSha512Input' = checkedShaInput sha512Incremental completeSha512Incremental

-- | Strict pairs.
data Pair a b = Pair !a !b

uncurry' :: (a -> b -> c) -> Pair a b -> c
uncurry' f (Pair a b) = f a b

-- | Inspired by `S.countInput`. The returned IO action can be run only
-- when the input stream is exhausted, otherwise an error occurs.
shaInput :: Decoder a -> (Decoder a -> Int -> Digest a)
  -> InputStream ByteString -> IO (InputStream ByteString, IO (Digest a))
shaInput increment end is = do
  ref <- newIORef $ Pair increment 0
  is' <- S.makeInputStream $ prod ref
  return $! (is', readIORef ref >>= uncurry' complete)

  where

  prod ref = do
    mbs <- S.read is
    maybe
      (return Nothing)
      (\bs -> (modifyRef ref (uncurry' $ modify bs)) >> (return $! Just bs))
      mbs

  complete decoder c = return $! end decoder c
  modify bs decoder c = Pair (pushChunk decoder bs) (c + (fromIntegral $ C.length bs))

-- | This returns an input stream exactly as the one being wrapped, but throws
-- an error if the computed SHA hash does not match the one given.
checkedShaInput :: Decoder a -> (Decoder a -> Int -> Digest a)
  -> String -> InputStream ByteString -> IO (InputStream ByteString)
checkedShaInput increment end digest is = do
  ref <- newIORef $ Pair increment 0
  is' <- S.makeInputStream $ prod ref
  return $! is'

  where

  prod ref = do
    mbs <- S.read is
    maybe
      (do r <- readIORef ref
          digest' <- uncurry' complete r
          if digest == showDigest digest'
            then return Nothing
            else throwIO UnmatchedSHAException)
      (\bs -> (modifyRef ref (uncurry' $ modify bs)) >> (return $! Just bs))
      mbs

  complete decoder c = return $! end decoder c
  modify bs decoder c = Pair (pushChunk decoder bs) (c + (fromIntegral $ C.length bs))

-- | Taken from System.IO.Streams.ByteString.
{-# INLINE modifyRef #-}
modifyRef :: IORef a -> (a -> a) -> IO ()
modifyRef ref f = do
    x <- readIORef ref
    writeIORef ref $! f x

-- | Exception raised by `checkedShaInput`.
data UnmatchedSHAException = UnmatchedSHAException
  deriving (Typeable)

instance Show UnmatchedSHAException where
    show _ = "Unmatched SHA exception."

instance Exception UnmatchedSHAException
