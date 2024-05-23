run:
	@cabal run -v0 --

install:
	cabal update && cabal install --overwrite-policy=always

clean:
	rm -rf build dist-newstyle .cabal* .ghc.environment*

define compile_from_source
    @rm -rf source_dir source.tar.gz
	@wget -O source.tar.gz $(1)
    @mkdir source_dir && tar xvf source.tar.gz -C source_dir --strip-components=1
    @cd source_dir && ./configure --prefix=$$HOME/.local && make -j && make install
    @rm -rf source_dir source.tar.gz
endef

install_requirements:
	$(call compile_from_source, https://libsdl.org/projects/SDL_image/release/SDL2_image-2.0.5.tar.gz)