#!/bin/bash

NAME=less-arbitrary

make;
open out/${NAME}.pdf &

while inotifywait $(cat inputs.list) ${NAME}.bib -e modify,close_write,attrib,access; do make -f Makefile2; done
