GENERATED=generated
BNFC=/home/students/inf/PUBLIC/MRJP/bin/bnfc
# BNFC=bnfc

all:
	mkdir -p $(GENERATED)
	$(BNFC) Latte.cf --functor --haskell -m -o $(GENERATED)
	cd $(GENERATED) && make
	cabal build
	# cp dist-newstyle/build/x86_64-linux/ghc-9.0.2/latte-0.1.0.0/x/interpreter/build/interpreter/interpreter .
	cp dist-newstyle/build/x86_64-linux/ghc-8.8.4/latte-0.1.0.0/x/interpreter/build/interpreter/interpreter .

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
