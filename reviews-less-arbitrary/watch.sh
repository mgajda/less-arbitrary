#!/bin/bash

NAME=haskell_sympo_2020

buildIt() {
  pandoc --from markdown+yaml_metadata_block ${NAME}.md -o ${NAME}.pdf
}

buildIt
xdg-open out/${NAME}.pdf &

while inotifywait ${NAME}.md -e modify,close_write,attrib,access; do buildIt; done
