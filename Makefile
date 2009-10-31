all: test

build:
	mkdir -p bin obj
	ghc --make -Wall -prof -auto-all -o bin/spell-correct -outputdir obj -iHUnit-1.0 *.hs

clean:
	rm -r bin obj

test: build
	bin/spell-correct
