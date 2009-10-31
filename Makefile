all: test

build:
	mkdir -p bin obj
	ghc --make -Wall -prof -auto-all -o bin/tests -outputdir obj -iHUnit-1.0 -main-is Tim.Spell.Correct.Tests.main spell-correct.hs Tests.hs
	ghc --make -Wall -prof -auto-all -o bin/demo -outputdir obj -main-is Tim.Spell.Correct.Demo.main spell-correct.hs Demo.hs

clean:
	rm -r bin obj

test: build
	bin/tests
