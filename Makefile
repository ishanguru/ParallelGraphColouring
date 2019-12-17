ifndef PNAME
override PNAME = graph_colouring.hs
endif

all:
	rm -rf output
	mkdir output
	stack ghc -- -O2 -threaded -rtsopts -eventlog $(PNAME)
