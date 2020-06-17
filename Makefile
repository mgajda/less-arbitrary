#SRC=plan.md
NAME=towards-better-union
SRC=towards-better-union.md
PDF=out/${NAME}.pdf
LHS=out/${NAME}.lhs
LHSPP=out/${NAME}.lhs.tex
#TPDF=out/${SRC:.md=_te.pdf}
HTML=out/${NAME}.html
TEX=out/${NAME}.tex
DOCX=out/${NAME}.docx
REFDOCX=acm_submission_template.docx
PICS=offset-array-example.eps
OPTIONS=--mathml				\
	--filter pandoc-crossref		\
 	--filter pandoc-citeproc		\
	--filter pandoc-hide-codeblocks		\
	--highlight-style=haddock		\
	--from=markdown+tex_math_dollars+yaml_metadata_block+footnotes+yaml_metadata_block \
	--columns 40 \
	--csl acm-template/acm-sig-proceedings.csl \
	--filter pandoc-filter-graphviz		
#	--listings \
#	--filter pandoc-citeproc		\

#TEMPLATE=acm-template/new-template.tex
TEMPLATE=springer-template/template.tex
GNUPLOT=gnuplot

all: out ${PDF} ${HTML} ${TEX} ${TPDF} ${DOCX} debug.out

# Build a number of formats into the `out/` directory. These will get published via Github pages.
out:
	mkdir -p out

offset-array-example.eps: offset-array-example.svg
	rsvg-convert --format eps offset-array-example.svg -o offset-array-example.eps --dpi-x 300 --dpi-y 300

#SRC_PLOTS=${wildcard plots/*.gnuplot}
#IMG_PLOTS=${subst plots/,out/,${SRC_PLOTS:.gnuplot=.svg} } ${PICS}
IMG_PLOTS=
BIBLIO=${NAME}.bib

out/%.svg: plots/%.gnuplot plots/%.csv
	$(GNUPLOT) $< > $@

debug.out:
	pandoc ${OPTIONS} \
		-t json -o debug.json ${SRC}
	json_pp < debug.json > debug_pp.json

${TEX}: ${IMG_PLOTS} ${SRC} ${TEMPLATE} ${BIBLIO}
	pandoc ${OPTIONS} \
		--template=${TEMPLATE}			\
		--pdf-engine=xelatex                    \
		--csl=templates/acm-sig-proceedings.csl	\
		--standalone \
		-o ${TEX} ${SRC}

${DOCX}: ${IMG_PLOTS} ${SRC} ${TEMPLATE} ${REFDOCX}
	pandoc ${OPTIONS} \
	       --reference-doc ${REFDOCX} \
		-o ${DOCX} ${SRC}

${PDF}: ${IMG_PLOTS} ${SRC} ${TEMPLATE} ${BIBLIO}
	pandoc ${OPTIONS} \
		--template=${TEMPLATE}		\
		--pdf-engine=xelatex		\
	        --csl=templates/acm-sig-proceedings.csl	\
		-o ${PDF} ${SRC}

#${LHS}: ${IMG_PLOTS} ${SRC} ${TEMPLATE} ${BIBLIO}
#	pandoc ${OPTIONS} \
#		--template=${TEMPLATE}		\
#		--pdf-engine=xelatex		\
#	--csl=templates/acm-sig-proceedings.csl	\
#	        --to=latex+lhs \
#		-o ${LHS} ${SRC}

#${LHSPP}: ${LHS}
#	lhs2TeX ${LHS} -o ${LHSPP}

#${PDF}: ${LHSPP} ${IMG_PLOTS}
#	xelatex ${LHSPP}

${HTML}: ${IMG_PLOTS} ${SRC} ${BIBLIO}
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
