run:
	cabal run -v0
	dot files.dot -Tpng > files.png
	eog files.png 2>/dev/null
