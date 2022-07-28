#!/bin/bash

NAME=prezi

make() {
  pandoc ${NAME}.md -o ${NAME}.tex -t beamer --standalone
  pandoc ${NAME}.md -o ${NAME}.pdf -t beamer
}

make;
xdg-open ${NAME}.pdf &

while inotifywait ${NAME}.md -e modify,close_write,attrib,access; do
  make ${NAME}.html;
done
