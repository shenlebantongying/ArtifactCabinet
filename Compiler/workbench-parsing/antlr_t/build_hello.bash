#!/bin/bash
set -e

# quirk to expand alias
shopt -s expand_aliases

alias antlr4='java -Xmx500M -cp "/usr/local/lib/antlr-4.9-complete.jar:$CLASSPATH" org.antlr.v4.Tool'
alias grun='java -Xmx500M -cp "/usr/local/lib/antlr-4.9-complete.jar:$CLASSPATH" org.antlr.v4.gui.TestRig'

mkdir  -p gen
antlr4 Hello.g4 -o gen
cd ./gen
javac Hello*.java
echo "Type 'hello nice'  then ctrl-d"
grun Hello r -gui -tree
