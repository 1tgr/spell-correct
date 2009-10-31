all: test

build:
	mkdir -p bin obj
	ghc --make -Wall -fno-warn-missing-signatures -prof -auto-all -o bin/tests -outputdir obj -iHUnit-1.0 -main-is Tim.Spell.Tests.main Correct.hs Tests.hs
	ghc --make -Wall -fno-warn-missing-signatures -prof -auto-all -o bin/demo -outputdir obj -main-is Tim.Spell.Demo.main Correct.hs Demo.hs

clean:
	rm -r bin obj

test: build
	bin/tests
