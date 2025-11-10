.PHONY: clean

UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Darwin)
	CC = clang
	HOMEBREW_PREFIX = /opt/homebrew
	READLINE_CFLAGS = -I/opt/homebrew/opt/readline/include
	READLINE_LDFLAGS = -L/opt/homebrew/opt/readline/lib -lreadline
	SDL_CFLAGS = -I$(HOMEBREW_PREFIX)/include
	SDL_LDFLAGS = -L$(HOMEBREW_PREFIX)/lib -lSDL3 -lSDL3_ttf
endif

ifeq ($(UNAME_S),Linux)
	CC = gcc
	#CC = tcc
	READLINE_LDFLAGS = -lreadline
	SDL_CFLAGS = $(shell pkg-config --cflags sdl3) -ISDL_ttf/include
	SDL_LDFLAGS = -L$(shell pkg-config --variable=libdir sdl3) -lSDL3 -LSDL_ttf/build -lSDL3_ttf -Wl,-rpath,SDL_ttf/build
endif

CFLAGS = -g $(READLINE_CFLAGS) $(SDL_CFLAGS)
LDFLAGS = -lm $(READLINE_LDFLAGS) $(SDL_LDFLAGS)

lisp-sdl3: lisp-sdl3.c

clean:
	rm -rf lisp-sdl3 lisp-sdl3.dSYM
