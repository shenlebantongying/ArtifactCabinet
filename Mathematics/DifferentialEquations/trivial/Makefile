asy_in=$(wildcard *.asy)
asy_out=$(asy_in:.asy=.pdf)

all: trivial_problems.pdf

trivial_problems.pdf: trivial_problems.tex $(asy_out)
	contextjit $< # makefile shortcut, first target

%.pdf: %.asy
	asy -f pdf $^

clean:
	rm *.pdf *.tuc *.log
