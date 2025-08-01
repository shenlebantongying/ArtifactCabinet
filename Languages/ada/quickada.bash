#!/bin/bash

adarun(){
      gnatmake "$1"
      echo "=[ RUN ]====>"
      rcname=$(basename -- $1)
      ./"${rcname%.*}"
}
