#!/bin/bash
set -e
inkscape -D $1 -o $2 --export-type=eps

# FIXME: broken
# inkscape -D $1 -o $2 --export-latex
