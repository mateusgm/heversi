
src_hs	=  $(shell find src -name "*.hs" -follow)

bin/othello : $(src_hs)
	ghc -O2 -isrc --make src/Main.hs -o bin/othello


.PHONY : doc
doc :
	@echo "* Building documentation"
	@haddock -h -o doc/haddock --optghc=-isrc --ignore-all-exports $(src_hs)
	

.PHONY : clean
clean :
	@echo "* Cleaning out binaries"	
	@rm -f bin/othello
	@find .    -name "*.hi" \
		-o -name "*.o" \
		-o -name "*~" \
		| xargs rm -f
