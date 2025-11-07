.PHONY: clean

a.out: tinylisp-sdl3.c
	tcc -g $(shell pkg-config --cflags sdl3) -ISDL_ttf/include tinylisp-sdl3.c -lreadline -lm -L$(shell pkg-config --variable=libdir sdl3) -lSDL3 -LSDL_ttf/build -lSDL3_ttf -Wl,-rpath,SDL_ttf/build

clean:
	rm -f a.out*
