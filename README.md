<!-- Sourcehut uses CommonMark + an HMTL subset.

I think the table tag can help recreate GitHub's nice boxes syntax:
```
> [!WARNING]
> This is not supported in CommonMark. It's a GitHub extension.
> See https://github.com/orgs/community/discussions/16925
```
-->
# A mini Lisp in C with a graphical canvas attached

<table>
    <tr><th>⚠️  Experimental</th><tr/>
    <tr><td>This is unknown territory and possibly broken. PRs welcome.</td></tr>
</table>

Based on the following repos:

* https://github.com/Robert-van-Engelen/tinylisp
* https://github.com/Robert-van-Engelen/lisp

All bugs found thus far have been my own. Tested on:

* Linux (Debian, Arch)
* Mac OS

# Prerequisites

1. Some C compiler. Compiles with [tcc](https://www.bellard.org/tcc).
2. GNU `make`
3. The following libraries
  * `SDL3` with development files
  * `SDL_ttf` with development files

On some platforms, the development files come with the libaries
by default. On others, you must get a [development
version](DEPENDENCIES.md#development-packages) of the package.

## Platform-Specific Instructions

This is incomplete, but such as we have:

* [Windows](DEPENDENCIES.md#windows) ([Visual Studio Code](DEPENDENCIES.md#visual-studio-code), [mingw-64](DEPENDENCIES.md#mingw-w64), and [MSYS2](DEPENDENCIES.md#msys2))
* [Mac](DEPENDENCIES.md#mac) (via Homebrew)
* [Linux](DEPENDENCIES.md#linux) distros:
  - [Arch-based](DEPENDENCIES.md#arch-linux) distros (Arch Linux, Manjaro)
  - [Debian-based](DEPENDENCIES.md#debian) distros (Debian, Linux Mint Debian Edition, Bunsen Labs)
  - [Ubuntu-based](DEPENDENCIES.md#ubuntu) distros (Pop!_OS, Linux Mint, ElementaryOS)

# Compile

```
$ make
```

# Run

```
$ ./lisp-sdl3
```

You should see a second window open up with a black graphical canvas. The
terminal will show a REPL after a list of available commands.

```
Type Lisp expressions or '(quit)' to quit.
>
```

# Examples

## Drawing a Line

Try typing this at the REPL
  ```
  (def draw ()
    (color 255 0 0)
    (line 100 100 200 300))
  ```

You should see a red line drawn on the canvas window.

## Loading Code from Files

You can load code from files, including your own.

_Option #1:_ Provide a filename at the commandline:
  ```
  $ ./lisp-sdl3 demo.lisp
  ```

_Option #2:_ Enter the following at the REPL:
  ```
  (load "demo.lisp")
  ```

You should see a screen with suggested next steps for handling input.
