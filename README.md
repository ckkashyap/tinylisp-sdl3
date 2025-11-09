# A mini Lisp in C with a graphical canvas attached

⚠️ Experimental

This is unknown territory and possibly broken. PRs welcome.

Based on https://github.com/Robert-van-Engelen/tinylisp and
https://github.com/Robert-van-Engelen/lisp. All bugs found thus far have been
my own.

Only tested so far on Linux and Mac OS.

# Prerequisites

* `SDL3`
* `SDL_ttf`

Install these from the infrastructure for your OS (`apt`, `pacman`, `brew`,
etc.) or build from source.

# Compile

```
$ make
```

# Run

```
$ ./lisp-sdl3
```

You should see a second window open up with a black graphical canvas. The
terminal will show a REPL.

# Example

Try typing this at the REPL:

```
(defun draw ()
  (color 255 0 0)
  (line 100 100 200 300))
```

Longer demo:

```
(load "demo.lisp")
```
