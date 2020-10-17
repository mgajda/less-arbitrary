#!/bin/bash

NAME=less-arbitrary

make;
xdg-open out/${NAME}.pdf &

while inotifywait $(cat inputs.list) \
                  towards-better-union.bib \
                  -e modify,close_write,attrib,access; do make -f Makefile; done
