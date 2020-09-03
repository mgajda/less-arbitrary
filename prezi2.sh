#!/bin/bash

NAME=prezi2

make() {
  pandoc ${NAME}.md --standalone -o ${NAME}.tex -t beamer
  pandoc ${NAME}.md -o ${NAME}.pdf -t beamer
}

make;
xdg-open ${NAME}.pdf &

while inotifywait ${NAME}.md -e modify,close_write,attrib,access; do
  make ${NAME}.html;
done
