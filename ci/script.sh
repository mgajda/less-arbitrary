#!/bin/bash

source ci/common.sh

message "Install packages"
apt-get update
#cd /build/
#ln -s /usr/share/zoneinfo/Europe/Warsaw /etc/localtime
apt-get install -y pandoc pandoc-citeproc make python-pandocfilters python-six python python3-sphinxcontrib.svg2pdfconverter texlive-xetex gnuplot-nox

message "Install pandoc-filter-graphviz"
git clone https://github.com/mgajda/pandoc-filter-graphviz.git pandoc-filter-graphviz
(cd pandoc-filter-graphviz; stack install)

message "Build document"
make
