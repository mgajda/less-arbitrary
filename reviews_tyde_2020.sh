#!/bin/bash

NAME=reviews_tyde_2020

buildIt() {
pandoc ${NAME}.md -o out/${NAME}.pdf
}

buildIt;
xdg-open out/${NAME}.pdf &

while inotifywait reviews_tyde_2020.md \
                  -e modify,close_write,attrib,access; do buildIt; done
