.PHONY: clean

# binary names distinct from source names for better tab-completion

small: tinylisp-sdl3.c
	tcc -g $(shell pkg-config --cflags sdl3) -ISDL_ttf/include tinylisp-sdl3.c -o small -lreadline -lm -L$(shell pkg-config --variable=libdir sdl3) -lSDL3 -LSDL_ttf/build -lSDL3_ttf -Wl,-rpath,SDL_ttf/build

large: lisp-sdl3.c
	tcc -g $(shell pkg-config --cflags sdl3) -ISDL_ttf/include lisp-sdl3.c -o large -lreadline -lm -L$(shell pkg-config --variable=libdir sdl3) -lSDL3 -LSDL_ttf/build -lSDL3_ttf -Wl,-rpath,SDL_ttf/build

clean:
	rm -f small* large*
