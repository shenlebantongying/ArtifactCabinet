#!/usr/bin/env bash

mkdir -p build && cd ./build
cmake .. -DCMAKE_INSTALL_PREFIX=./
make install
source prefix.sh # located in the build directory
./bin/kSimpleEditExe