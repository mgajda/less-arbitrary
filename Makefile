#SRC=plan.md
NAME=towards-better-union
SRC=${NAME}.md
PDF=out/${SRC:.md=.pdf}
#TPDF=out/${SRC:.md=_te.pdf}
HTML=out/${SRC:.md=.html}
TEX=out/${SRC:.md=.tex}
PICS=offset-array-example.eps
OPTIONS=--mathml				\
	--filter pandoc-crossref				\
	--filter pandoc-citeproc		\
	--highlight-style=haddock		\
	--from=markdown+tex_math_dollars+yaml_metadata_block+footnotes+yaml_metadata_block \
	--columns 40 \
	--csl acm-template/acm-sig-proceedings.csl \
	--filter pandoc-filter-graphviz		\
	--listings \

TEMPLATE=acm-template/new-template.tex
GNUPLOT=gnuplot

all: out ${PDF} ${HTML} ${TEX} ${TPDF} debug.out

# Build a number of formats into the `out/` directory. These will get published via Github pages.
out:
	mkdir -p out

offset-array-example.eps: offset-array-example.svg
	rsvg-convert --format eps offset-array-example.svg -o offset-array-example.eps --dpi-x 300 --dpi-y 300

#SRC_PLOTS=${wildcard plots/*.gnuplot}
#IMG_PLOTS=${subst plots/,out/,${SRC_PLOTS:.gnuplot=.svg} } ${PICS}
IMG_PLOTS=

out/%.svg: plots/%.gnuplot plots/%.csv
	$(GNUPLOT) $< > $@

debug.out:
	pandoc ${OPTIONS} \
		-t json -o debug.json ${SRC}

${TEX}: ${IMG_PLOTS} ${SRC} ${TEMPLATE}
	pandoc ${OPTIONS} \
		--template=${TEMPLATE}			\
		--csl=templates/acm-sig-proceedings.csl	\
		--standalone \
		-o ${TEX} ${SRC}

${PDF}: ${IMG_PLOTS} ${SRC} ${TEMPLATE}
	pandoc ${OPTIONS} \
		--template=${TEMPLATE}		\
		--pdf-engine=xelatex		\
		-o ${PDF} ${SRC}
#	--csl=templates/acm-sig-proceedings.csl	\

${HTML}: ${IMG_PLOTS} ${SRC}
	pandoc ${OPTIONS}		\
		--standalone		\
		--css=templates/pandoc.css	\
		--toc			\
		-o ${HTML} ${SRC}
	cp templates/pandoc.css out/pandoc.css

.PHONY: clean

clean:
	rm -rf out/

# vim: set ts=8 :
