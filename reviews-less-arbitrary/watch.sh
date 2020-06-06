#!/bin/bash

NAME=haskell-sympo-2020-A

buildIt() {
  pandoc ${NAME}.md -o ${NAME}.pdf
}

buildIt
xdg-open out/${NAME}.pdf &

while inotifywait ${NAME}.md -e modify,close_write,attrib,access; do buildIt; done
