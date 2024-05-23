run:
	@cabal run -v0
	@dot files.dot -Tpng > files.png
	@GTK_PATH="" eog files.png 2>/dev/null

install:
	cabal update && cabal install --overwrite-policy=always

clean:
	rm -rf build dist-newstyle .cabal* .ghc.environment*