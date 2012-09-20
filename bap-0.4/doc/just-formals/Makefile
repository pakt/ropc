PAPERNAME=vine
TEXFILES =  *.tex

TEXFILES = $(wildcard ../*.tex) $(wildcard *.sty)
BIBFILES = $(wildcard bibtex/*.bib) 

all: $(PAPERNAME).pdf 

$(PAPERNAME).pdf : $(TEXFILES) $(BIBFILES) $(PAPERNAME).tex 
	pdflatex $(PAPERNAME)
	pdflatex $(PAPERNAME)

clean:
	rm -f *.ps *.pdf *.dvi *.aux *.log *.blg *.bbl *~ *.ilg *.idx *.out *.in $(PROGS)
