#!/bin/bash

cleandir(){
    find . -type f -name '*.adb' -exec gnatclean '{}' \;
}

set -e

cleandir
cd ./imperative_language
cleandir

