#!/bin/sh

erlc r.erl
#                          | call init:stop(). after r:start() call
erl -noshell -s r start -s init stop
#               ^ call module r's start