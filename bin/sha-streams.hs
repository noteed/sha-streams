module Main (main) where

import System.Environment (getArgs)
import qualified System.IO.Streams as S

import Data.Digest.Pure.SHA
import System.IO.Streams.SHA

main :: IO ()
main = do
  [filename] <- getArgs
  d1 <- S.withFileAsInput filename $ \is -> do
    (is1, getSha1) <- sha1Input is
    (is224, getSha224) <- sha224Input is1
    (is256, getSha256) <- sha256Input is224
    (is384, getSha384) <- sha384Input is256
    (is512, getSha512) <- sha512Input is384
    S.skipToEof is512
    d1 <- getSha1
    putStrLn $ showDigest d1
    getSha224 >>= putStrLn . showDigest
    getSha256 >>= putStrLn . showDigest
    getSha384 >>= putStrLn . showDigest
    getSha512 >>= putStrLn . showDigest
    return d1

  -- This must throw an UnmatchedSHAException (unless `filename` above is
  -- "System/IO/Streams/SHA.hs").
  S.withFileAsInput "System/IO/Streams/SHA.hs" $ \is -> do
    (is1, _) <- sha1Input is
    is1' <- checkedSha1Input d1 is1
    S.skipToEof is1'
