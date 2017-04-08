#! /usr/bin/env bash

STACK_IMAGE=${1:-7.8.4}

docker run -t -i \
  -v `pwd`:/home/gusdev/sha-streams \
  images.reesd.com/reesd/stack:$STACK_IMAGE \
  cabal install sha-streams/sha-streams.cabal
