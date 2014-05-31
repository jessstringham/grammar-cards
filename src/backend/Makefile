
all:
	ghc Main.hs

picky:
	ghc -Wall Main.hs

hlint:
	hlint *.hs ./GrammarCards/Reader/*.hs ./GrammarCards/Processor/*.hs ./GrammarCards/Util/*.hs

clean:
	find . -name \*.o -delete
	find . -name \*.hi -delete
	rm -rf *.o *.hi Main

