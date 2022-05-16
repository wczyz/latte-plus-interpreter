GENERATED=generated

all:
	mkdir -p $(GENERATED)
	bnfc Latte.cf --functor --haskell -m -o $(GENERATED)
	cd $(GENERATED) && make
	cabal build
	# ghc --make Main.hs -o interpreter

test: 
	# good
	mkdir -p good/output
	for file in good/*.lat ; do \
		name=$$(basename $$file | sed -e 's/\.lat//g') ; \
		echo $$name ; \
		cabal exec interpreter < $$file > good/output/$$name.out ; \
		diff good/output/$$name.out good/expected-output/$$name.out ; \
	done
	# bad
	for file in bad/*.lat ; do \
		name=$$(basename $$file | sed -e 's/\.lat//g') ; \
		echo $$name ; \
		(cabal exec interpreter < $$file 2> /dev/null) && echo "TEST FAILED" || echo -n "" ; \
	done


clean:
	cabal clean
	rm -f *.o *.hi interpreter
	rm -rf good/output
	rm -rf $(GENERATED)
