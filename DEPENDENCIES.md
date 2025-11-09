# Platform-Specific Dependency Guides

<table>
    <tr><th>⚠️  Remember: These are Best-Effort!</th><tr/>
    <tr><td>Test reports and pull requests to improve these instructions are welcome.</td></tr>
</table>

## Overview

The following steps are elaborated on below:

1. Get [`tcc`](https://bellard.org/tcc/)
2. Get the dev versions of [SDL3](https://www.libsdl.org/) and [SDL_ttf](https://wiki.libsdl.org/SDL3_ttf/FrontPage)
3. `git clone` locally
4. `make`

## Dependencies

This project minimizes dependencies by using:
1. [GNU `make`](https://www.gnu.org/software/make/) to manage build
1. [`tcc`](https://bellard.org/tcc/) as the compiler
2. [SDL3](https://www.libsdl.org/) and [SDL_ttf](https://wiki.libsdl.org/SDL3_ttf/FrontPage)

### Development Packages

Some platforms have separate development and non-development packages for SDL and SDL3.

These include:

- Windows ([VS Code](#visual-studio-code))
- [Debian-based](#debian) Linux distributions (Debian, Linux Mint Debian Edition, Bunsen Labs)
- [Ubuntu-based](#ubuntu) Linux distributions (Pop!_OS, Linux Mint, ElementaryOS)


The [platform-specific instructions](#platform-specific-details)
elaborate with best-effort guidance on installing `make`, `tcc`,
and the libraries below.

<table>
    <tr><th>Library</th><th>Role</th><th>Project Page</th></tr>
    <tr>
        <td>SDL3</td>
        <td>Cross-platform windowing, input, and graphics</td>
        <td><a href= "https://libsdl.org/">Homepage</a></td>
    </tr>
    <tr>
        <td>SDL_ttf</td>
        <td>Loads and draws TrueType fonts</td>
        <td><a href="https://wiki.libsdl.org/SDL3_ttf/FrontPage">SDL Wiki page</a></td>
    </tr>
</table>


# Platforms


## Linux

The commands below will likely require use of `sudo`.

### Debian

Tested on [Debian](https://wiki.debian.org/)'s [Debian 13 (trixie)](https://wiki.debian.org/DebianTrixie) release:

```
apt install make tcc libsdl3-dev libsdl3-ttf-dev
```

<table>
<tr>
    <th>Packages:</th>
    <td><a href="https://packages.debian.org/trixie/make"><code>make</code></td>
    <td><a href="https://packages.debian.org/trixie/tcc"><code>tcc</code></a></td>
    <td><a href="https://packages.debian.org/trixie/libsdl3-dev"><code>libsdl3-dev</code>/a></td>
    <td><a href="https://packages.debian.org/trixie/libsdl3-ttf-dev"><code>libsdl3-ttf-dev</code></a></td>
</tr>
</table>

#### Notes

* You **must** use the `*-dev` packages to get development headers!
* These instructions *may* also work on Debian-based distros, including:
  - [LMDE (Linux Mint Debian Edition)](https://linuxmint.com/download_lmde.php)
* For users on Debian 12 (Bookworm / oldstable) or earlier, please see [compiling](#compiling)
  - Bookworm has no SDL3 packages
  - Debian-based distros using oldstable also lack SDL3 packages ([Bunsen Labs](https://www.bunsenlabs.org/), etc)

### Fedora

Untested:
```
dnf install make SDL3 SDL3_ttf
```

<table>
<tr>
    <th>Packages:</th>
    <td>(No <code>tcc</code> package?)</td>
    <td><a href="https://packages.fedoraproject.org/pkgs/make/make/"><code>make</code></a></td>
    <td><a href="https://packages.fedoraproject.org/pkgs/SDL3/SDL3/"><code>SDL3</code></a></td>
    <td><a href="https://packages.fedoraproject.org/pkgs/SDL3_ttf/SDL3_ttf/"><code>SDL3_ttf</code></a><td>
</tr>
</table>

Once those dependencies are installed, proceed to [compile `tcc` from source](#compiling-tcc])).

#### Notes

* Fedora appears to include developent headers in their packages
* These instructions *may* also work on Fedora-based distros, including:
  - [Nobara Linux](https://nobaraproject.org/)


### Ubuntu

Untested:
```
apt install make tcc libsdl3-dev libsdl3-ttf-dev
```

<table>
<tr>
    <th>Packages:</th>
    <td><a href="https://packages.ubuntu.com/questing/make"><code>make</code></a></td>
    <td><a href="https://packages.ubuntu.com/questing/tcc"><code>tcc</code></a></td>
    <td><a href="https://packages.ubuntu.com/questing/libsdl3-dev"><code>libsdl3-dev</code>/a></td>
    <td><a href="https://packages.ubuntu.com/questing/libsdl3-ttf-dev"><code>libsdl3-ttf-dev</code></a></td>
</tr>
</table>

#### Notes

* You **must** use the `*-dev` packages to get development headers
* The packages linked above
* These instructions *may* also work on distros which uses packages from [Ubuntu Questing Quokka](https://packages.ubuntu.com/questing/), including:
  - [Pop!_OS](https://system76.com/pop/)
  - [Linux Mint](https://linuxmint.com/)
  - [ElementaryOS](https://elementary.io/)


### Arch Linux

<table>
    <tr><th>⚠️  Uses Third-Party Packages!</th><tr/>
    <tr><td>
        <a href="#compiling-the-dependencies">Compiling from source</a> may be safer.
        <br/>
        The <code>sdl3_ttf</code> package below is from the Arch User Repository (AUR),
        i.e. <a href="https://aur.archlinux.org/">"at your own risk"</a> per their
        own homepage.
    </td></tr>
</table>

```
pacman install make tcc sdl3 sdl3_ttf
```

*May also work on [Manjaro]() and other Arch-based distros.*

<table>
<tr>
    <th>Packages:</th>
    <td><a href="https://archlinux.org/packages/core/x86_64/make/"><code>make</code></a></td>
    <td><a href="https://archlinux.org/packages/extra/x86_64/tcc/"><code>tcc</code></a></td>
    <td><a href="https://archlinux.org/packages/extra/x86_64/sdl3/"><code>sdl3</code></a></td>
    <td>(⚠️  From <a href="https://aur.archlinux.org/">AUR</a>) <a href="https://aur.archlinux.org/packages/sdl3_ttf"><code>sdl3_ttf</code></a></td>
</tr>
</table>

#### Notes

* These instructions *may* work in Arch-based distros, including:
  - [Manjaro Linux](https://manjaro.org/)
  - [SteamOS](https://store.steampowered.com/steamos) (may require enabling `pacman`)


### Mac

Untested.

#### Installing Homebrew

The [Homebrew](https://brew.sh/) project now offers a pre-packaged
installer as a [.pkg](https://github.com/Homebrew/brew/releases/tag/4.6.20).
This is often a safer means of installin compared to piping from `curl`.

#### Installing Dependencies with Homebrew

Once installed, Homebrew should have packages for the dependencies
on recent macOS versions:

```
brew install make tcc sdl3 sdl3_ttf
```

<tr>
    <th>Packages:</th>
    <td><a href="https://formulae.brew.sh/formula/make#default"><code>make</code></a></td>
    <td><a href="https://formulae.brew.sh/formula/tcc#default">(⚠️  Marked deprecated) <code>tcc</code></a></td>
    <td><a href="https://formulae.brew.sh/formula/sdl3#default"><code>sdl3</code></a></td>
    <td><a href="https://formulae.brew.sh/formula/sdl3_ttf#default"><code>sdl3_ttf</code></a></td>
</tr>
</table>

#### Notes

Since Homebrew marks `tcc` as deprecated:
- it may be removed one day
- compiling from source may prevent future hassles

To do so, please:
1. Also install either `gcc` or `clang` to compile `tcc`:
   - `brew install gcc`
   - `brew install clang`
2. See [compiling `tcc`](#compiling-tcc) below


## Windows

There are multiple options for attempting build the project on Windows.

For pre-built binaries, the following are likely safest:

1. Install binaries manually (safest)
3. Try tools like [MSYS2](#msys2)

### Alternatives to Chocolatey

Chocolatey has:
- an outdated version of `tcc` (2015)
- no SDL3 packages
- additional security consderations

It is worth reading their documentation on
[community package safety](https://docs.chocolatey.org/en-us/community-repository/community-packages-disclaimer/). These are roughly the same as the AUR for Arch Linux as a source of [third-party packages](#third-party-packages).

#### Alternative 1: mingw-w64

The [mingw-w64](#mingw-w64) project offers an alternative
to the Microsoft C and C++ compilers.

#### Alternative 2: MSYS2

[MSYS2](#msys2) is heavier than mingw-w64, yet it comes with GNU `make`.

It offers a dedicated terminal environment with a more *NIX-like
approach than Powershell:

- Runs atop the Windows kernel
- Comes with GNU `make` and `gcc` (C compiler which can build `tcc`)
- Avoids [the trouble WSL2 seems to cause](#wsl2)


### GNU `make`

Ports of GNU `make` for Windows are available via the following:

1. [mingw-w64](https://www.mingw-w64.org/)
2. [Chocolatey](https://chocolatey.org/) (See [risk overview above](#chocolatey-considerations))

The [WSL and WSL2](#wsl)


### The `tcc` Compiler for Windows

<table>
    <tr><th>ℹ️  The GNU Savannah builds may be behind the latest forks.</th><tr/>
    <tr><td>Although later forks exist, the GNU builds are the likeliest to be safe to use.</td></tr>
</table>

GNU's Savannah [download page for `tcc`.](https://download.savannah.gnu.org/releases/tinycc/)
hosts multiple builds.

The steps likely include:

1. Download a version (GNU's is [`tcc-0.9.27-win64-bin.zip`](https://download.savannah.gnu.org/releases/tinycc/tcc-0.9.27-win64-bin.zip)
1. Extract the compiler to a folder you can add to `PATH`
2. [Add it to `PATH` variable](https://stackoverflow.com/questions/9546324/adding-a-directory-to-the-path-environment-variable-in-windows)

Afterward, you may need to restart or log out + in.


### SDL Binaries

The SDL team offers official release builds from GitHub.

<table>
    <tr><th>ℹ️  You may need to click to <em>"Show all <code>$N</code> Assets"</em></th><tr/>
    <tr><td>
        <ol>
            <li>GitHub's release pages hide long lists of files</li>
            <li><code>SDL3*-devel-$VERSION-$VARIANT*.zip</code> gets hidden</li>
        </ol>
    </td></tr>
</table>


#### Visual Studio Code

These take the form `SDL3*-devel-$VERSION-VC.zip`.

Get the latest for the following:

1. The latest `SDL3-devel-$VERSION-VC.zip` on the ([`SDL3` releases page](https://github.com/libsdl-org/SDL_ttf/releases))
2. The latest `SDL3_ttf-devel-$VERSION-VC.zip` on the ([`SDL3_ttf`releases page](https://github.com/libsdl-org/SDL_ttf/releases)

See the included `README.md` and `INSTALL.md` in each file for more details.


#### mingw-w64

This project offers an alterantive compiler and some Unix-like
wrappers over Windows APIs.

The following groups may find it interesting:
1. Experienced developers
2. Anyone determined lower their CPU and RAM use
   - VS Code is famously resource-intensive
   - WSL is also resource-intensive

After installing [mingw-w64](https://www.mingw-w64.org/), the
you will need the following packages:

1. The latest `SDL3-devel-$VERSION-mingw.zip` on the ([`SDL3` releases page](https://github.com/libsdl-org/SDL_ttf/releases))
2. The latest `SDL3_ttf-devel-$VERSION-mingw.zip` on the ([`SDL3_ttf`releases page](https://github.com/libsdl-org/SDL_ttf/releases)


#### MSYS2

The [MSYS2](https://www.msys2.org/) project may offer in-box
builds of GNU utilities.

It has a Unix-like command line calld `mintty` and a Cygwin-based
Unix-like environment with pacman for package management.

See [What is MSYS2?](https://www.msys2.org/docs/what-is-msys2/) to
learn more.


#### WSL2?

GUI supprt in WSL2 is better than it used to be, but there are still problems.

<table>
    <tr><th>⚠️  There are Known Issues</th><tr/>
    <tr><td>
        The GitHub issues queue for SDL3 has multiple
        <!-- Putting this on one line since vim's syntax highlighting breaks otherwise. -->
        <a href="https://github.com/search?q=repo%3Alibsdl-org%2FSDL+wsl2&type=issues">WSL-related issues,</a>
        some of which have been open for years.
    </tr></td>
</table>

If you would like to try it anyway, you may need to consult
the following:

1. setting up a WSL(2?) environemnt
2. running the [Ubuntu-specific commands above](#ubuntu)
3. tinkering with your GUI config

The following may help:
- [SDL Discourse Forums](https://discourse.libsdl.org/)
- [SDL GitHub Issues](https://github.com/libsdl-org/SDL/issues)
- [WSL GitHub issues](https://github.com/microsoft/wsl/issues)
- [Official WSL Documentation](https://learn.microsoft.com/en-us/windows/wsl/)


# Incomplete Platform Info

There are two options available if your platform does not have
instructions listed:

1. Double-checking for prebuilt binaries
2. Compiling them yourself


## Where to Check for Binaries?

First, double-checking for pre-built binaries:
- The [GNU builds of `tcc`](https://download.savannah.gnu.org/releases/tinycc/) or your package manager
- The [`SDL3` releases page](https://github.com/libsdl-org/SDL/releases)
- The [`SDL3_ttf` releases page](https://github.com/libsdl-org/SDL_ttf/releases)

If there are no binaries, please see the following for guidance:

* any sections below
* the homepage of the project itself

## Compiling

### Compiling `tcc`

In the unlikely event neither the GNU download page or
your package manager have prebuilt `tcc`, try:

1. Download [the source (`tcc-0.9.27.tar.bz2`)]() (via the [GNU download page](https://download.savannah.gnu.org/releases/tinycc/))
2. Compile with another C compiler (`clang` or `gcc` both work)

For usage info, please see [the `tcc` documentation](https://bellard.org/tcc/tcc-doc.html) for more information.

*In theory, you could also use  `gcc`, `clang`, or another compiler to build the project.`

### Compiling SDL3 and SDL3_ttf

The SDL team offers excellent documentation on compiling from source:

- [SDL3's `INSTALL.md`](https://github.com/libsdl-org/SDL/blob/main/INSTALL.md)
- [SDL3_ttf's `INSTALL.md`](https://github.com/libsdl-org/SDL_ttf/blob/main/INSTALL.md)

# Additional Notes

## Third-Party Packages

Both of these consist of third-party packages:

- The Arch User Repository (AUR)
- Chocolatey, a package manager for Windows

As the [Arch User Repository (AUR) homepage states](https://aur.archlinux.org/):

> **at your own risk.**

Some Arch users may deem it worth the risk. If you are cautious enough to
prefer the [Debian approach](https://wiki.debian.org/DontBreakDebian),
avoiding  the risks will come naturally.

