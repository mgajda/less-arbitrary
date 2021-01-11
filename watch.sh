#!/bin/bash

NAME=towards-better-union

make;
xdg-open out/${NAME}.pdf &

while inotifywait $(cat inputs.list) ${NAME}.bib -e modify,close_write,attrib,access; do make; done
