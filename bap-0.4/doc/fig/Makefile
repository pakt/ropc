FIGS = $(wildcard *.fig)

all: $(FIGS:.fig=.pdf)

%.pdf : %.fig
	fig2dev -L pdf $< $@

clean:
	rm -f $(FIGS:.fig=.pdf)