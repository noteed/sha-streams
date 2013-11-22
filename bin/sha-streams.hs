module Main (main) where

import qualified System.IO.Streams as S

import Data.Digest.Pure.SHA
import System.IO.Streams.SHA

main :: IO ()
main = do
  S.withFileAsInput "bin/sha-streams.hs" $ \is -> do
    (is1, getSha1) <- sha1Input is
    (is224, getSha224) <- sha224Input is1
    (is256, getSha256) <- sha256Input is224
    (is384, getSha384) <- sha384Input is256
    (is512, getSha512) <- sha512Input is384
    S.skipToEof is512
    getSha1 >>= putStrLn . showDigest
    getSha224 >>= putStrLn . showDigest
    getSha256 >>= putStrLn . showDigest
    getSha384 >>= putStrLn . showDigest
    getSha512 >>= putStrLn . showDigest
