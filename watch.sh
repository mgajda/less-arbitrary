#!/bin/bash

NAME=towards-better-union

make;
open out/${NAME}.pdf &

while inotifywait ${NAME}.md ${NAME}.bib -e modify,close_write,attrib,access; do make; done
