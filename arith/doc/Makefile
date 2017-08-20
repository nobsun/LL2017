NAME	= README
STY     = slide
VIEWER  = acroread

all	: $(NAME).pdf

ifeq ($(STY),slide)
# %.pdf	: %.md
# 	pandoc -f markdown+lhs -t beamer+lhs -V theme:Madrid -H slide.tex -o $@ --latex-engine=lualatex $<
# %.pdf	: %.md
# 	pandoc -f markdown -t beamer -V theme:Singapore -V fontsize:12pt -H slide.tex -o $@ --latex-engine=lualatex $<
# %.pdf	: %.md slide.tex Makefile
# 	pandoc -f markdown -t beamer -V theme:Madrid -V colortheme:albatross -V fontsize:12pt -H slide.tex -o $@ --latex-engine=lualatex $<
%.pdf	: %.md slide.tex Makefile
	pandoc -f markdown -t beamer -V theme:Madrid -V fontsize:12pt -H slide.tex -o $@ --latex-engine=lualatex $<
else
%.pdf	: %.tex
	platex $(NAME)
	dvipdfmx $(NAME)
endif

clean	:
	-rm *~ *.nav *.out *.snm *.dvi *.aux *.log *.bib *.bbl *.blg *.toc *.ptb *.pdf

view	: $(NAME).pdf
	$(VIEWER) $< &
