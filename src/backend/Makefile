
all:
	ghc GrammarCardsGenerator.hs

picky:
	ghc -Wall GrammarCardsGenerator.hs

hlint:
	hlint *.hs ./GrammarCards/Reader/*.hs ./GrammarCards/Processor/*.hs ./GrammarCards/Util/*.hs

clean:
	find . -name \*.o -delete
	find . -name \*.hi -delete
	rm -rf *.o *.hi GrammarCardsGenerator

