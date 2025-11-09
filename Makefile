.PHONY: clean

UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Darwin)
	CC = clang
	HOMEBREW_PREFIX = /opt/homebrew
	SDL_CFLAGS = -I$(HOMEBREW_PREFIX)/include
	SDL_LIBS = -L$(HOMEBREW_PREFIX)/lib -lSDL3 -lSDL3_ttf
endif

ifeq ($(UNAME_S),Linux)
	CC = gcc
	#CC = tcc
	SDL_CFLAGS = $(shell pkg-config --cflags sdl3) -ISDL_ttf/include
	SDL_LIBS = -L$(shell pkg-config --variable=libdir sdl3) -lSDL3 -LSDL_ttf/build -lSDL3_ttf -Wl,-rpath,SDL_ttf/build
endif

lisp-sdl3: lisp-sdl3.c
	$(CC) -g $(SDL_CFLAGS) lisp-sdl3.c -o lisp-sdl3 -lreadline -lm $(SDL_LIBS)

clean:
	rm -rf lisp-sdl3 lisp-sdl3.dSYM
