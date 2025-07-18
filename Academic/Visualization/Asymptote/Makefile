msrc=$(wildcard *.asy)
pdfs=$(msrc:.asy=.pdf)

all: $(pdfs)

%.pdf: %.asy
	asy -f pdf $^

