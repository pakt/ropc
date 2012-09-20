PAPERNAME=main
TEXFILES =  *.tex

TEXFILES = $(wildcard *.tex) $(wildcard *.sty)\
	 $(wildcard chap-intro/*.tex) $(wildcard chap-intro/*.sty)\
	$(wildcard chap-formalism/*.tex) $(wildcard chap-formalism/*.sty)\
	$(wildcard chap-tools/*.tex) $(wildcard chap-tools/*.sty)\
	$(wildcard chap-examples/*.tex) $(wildcard chap-examples/*.sty)\
	$(wildcard fig/*.pdf)


BIBFILES = $(wildcard bibtex/*.bib) 

all: $(PAPERNAME).pdf 

$(PAPERNAME).pdf : $(TEXFILES) $(BIBFILES) $(PAPERNAME).tex 
	pdflatex $(PAPERNAME)
	bibtex $(PAPERNAME)
	pdflatex $(PAPERNAME)
	pdflatex $(PAPERNAME)

clean:
	rm -f *.ps *.pdf *.dvi *.aux *.log *.blg *.bbl *~ *.ilg *.idx *.out *.in $(PROGS)
