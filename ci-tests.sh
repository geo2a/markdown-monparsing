#!/bin/sh

cabal configure --enable-tests && cabal build && cabal test
./dist/build/Commom-Parsers-Test/Commom-Parsers-Test