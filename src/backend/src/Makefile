
all:
	ghc src/Main.hs

picky:
	ghc -Wall Main.hs

hlint:
	hlint *.hs ./LanguageCenter/Reader/*.hs ./LanguageCenter/Processor/*.hs ./LanguageCenter/Util/*.hs

clean:
	find . -name \*.o -delete
	find . -name \*.hi -delete
	rm -rf *.o *.hi Main

