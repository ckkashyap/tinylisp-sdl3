# Platform-Specific Dependency Guide

**⚠️  Under construction!**

If your experience differs or adds to this guide, please [share it](http://akkartik.name/contact)
or upload it anywhere you find convenient.

## Overview

1. Get dev versions (with C header files) of [SDL3](https://www.libsdl.org/) and [SDL_ttf](https://wiki.libsdl.org/SDL3_ttf/FrontPage)
   and [SDL_ttf](https://wiki.libsdl.org/SDL3_ttf/FrontPage) packages.
2. Get dev versions of the readline package.
3. `git clone` locally
4. `make`

### Development Packages

Some platforms have separate development and non-development packages for SDL
and SDL3. Development packages come with C headers; now one can use them in
new programs. Non-development versions are for using software without hacking
on software.

Platforms with separate development packages:

- Windows ([VS Code](#visual-studio-code))
- [Debian-based](#debian) Linux distributions (Debian, Linux Mint Debian Edition, Bunsen Labs)
- [Ubuntu-based](#ubuntu) Linux distributions (Pop!_OS, Linux Mint, ElementaryOS)


# Platforms


## Linux

The commands below will likely require use of `sudo`.

### Debian

Tested on [Debian](https://wiki.debian.org/)'s [Debian 13 (trixie)](https://wiki.debian.org/DebianTrixie) release:

```
apt install make libsdl3-dev libsdl3-ttf-dev
```

<table>
<tr>
    <th>Packages:</th>
    <td><a href="https://packages.debian.org/trixie/make"><code>make</code></td>
    <td><a href="https://packages.debian.org/trixie/libsdl3-dev"><code>libsdl3-dev</code>/a></td>
    <td><a href="https://packages.debian.org/trixie/libsdl3-ttf-dev"><code>libsdl3-ttf-dev</code></a></td>
    <td><a href="https://packages.debian.org/trixie/tcc"><code>tcc</code></a> (optional)</td>
</tr>
</table>

#### Notes

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
    <td><a href="https://packages.fedoraproject.org/pkgs/make/make/"><code>make</code></a></td>
    <td><a href="https://packages.fedoraproject.org/pkgs/SDL3/SDL3/"><code>SDL3</code></a></td>
    <td><a href="https://packages.fedoraproject.org/pkgs/SDL3_ttf/SDL3_ttf/"><code>SDL3_ttf</code></a><td>
</tr>
</table>

#### Notes

* These instructions *may* also work on Fedora-based distros, including:
  - [Nobara Linux](https://nobaraproject.org/)


### Ubuntu

Untested:

```
apt install make libsdl3-dev libsdl3-ttf-dev
```

<table>
<tr>
    <th>Packages:</th>
    <td><a href="https://packages.ubuntu.com/questing/make"><code>make</code></a></td>
    <td><a href="https://packages.ubuntu.com/questing/libsdl3-dev"><code>libsdl3-dev</code>/a></td>
    <td><a href="https://packages.ubuntu.com/questing/libsdl3-ttf-dev"><code>libsdl3-ttf-dev</code></a></td>
    <td><a href="https://packages.ubuntu.com/questing/tcc"><code>tcc</code></a> (optional)</td>
</tr>
</table>

#### Notes

* These instructions *may* also work on distros which uses packages from [Ubuntu Questing Quokka](https://packages.ubuntu.com/questing/), including:
  - [Pop!_OS](https://system76.com/pop/)
  - [Linux Mint](https://linuxmint.com/)
  - [ElementaryOS](https://elementary.io/)


### Arch Linux

```
pacman -S make readline sdl3
```

<table>
<tr>
    <th>Packages:</th>
    <td><a href="https://archlinux.org/packages/core/x86_64/make/"><code>make</code></a></td>
    <td><a href="https://archlinux.org/packages/extra/x86_64/sdl3/"><code>sdl3</code></a></td>
    <td>(⚠️  From <a href="https://aur.archlinux.org/">AUR</a>) <a href="https://aur.archlinux.org/packages/sdl3_ttf"><code>sdl3_ttf</code></a></td>
    <td><a href="https://archlinux.org/packages/extra/x86_64/tcc/"><code>tcc</code></a> (optional)</td>
</tr>
</table>

#### Notes

* ⚠️  SDL3_ttf is only available in the community supported [AUR](https://aur.archlinux.org/) which is less trustworthy. Use at your own risk, and consider [compiling from source](#compiling-the-dependencies). It's pretty easy if you have even a little bit of experience.
* These instructions *may* work in other Arch-based distros, including:
  - [Manjaro Linux](https://manjaro.org/)
  - [SteamOS](https://store.steampowered.com/steamos) (may require enabling `pacman`)


### Mac

First install Xcode (`xcode-select --install`) and [Homebrew](https://brew.sh/). Then:

```
brew install readline sdl3 sdl3_ttf
```

<tr>
    <th>Packages:</th>
    <td><a href="https://formulae.brew.sh/formula/make#default"><code>make</code></a></td>
    <td><a href="https://formulae.brew.sh/formula/sdl3#default"><code>sdl3</code></a></td>
    <td><a href="https://formulae.brew.sh/formula/sdl3_ttf#default"><code>sdl3_ttf</code></a></td>
</tr>
</table>

## Windows

There are multiple options for attempting build the project on Windows, in two broad categories:

1. Install binaries manually (safest)
2. Try tools like Chocolatey, mingw-w64 or MSYS2

### Where to Check for Binaries

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

These take the form `SDL3\*-devel-$VERSION-VC.zip`.

Get the latest for the following:

1. The latest `SDL3-devel-$VERSION-VC.zip` on the ([`SDL3` releases page](https://github.com/libsdl-org/SDL_ttf/releases))
2. The latest `SDL3_ttf-devel-$VERSION-VC.zip` on the ([`SDL3_ttf` releases page](https://github.com/libsdl-org/SDL_ttf/releases)

See the included `README.md` and `INSTALL.md` in each file for more details.


#### mingw-w64

This project offers an alternative compiler and some Unix-like
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


### Chocolatey

Chocolatey has:
- no SDL3 packages
- additional security consderations

It is worth reading their documentation on [community package safety](https://docs.chocolatey.org/en-us/community-repository/community-packages-disclaimer/). These are roughly as trustworthy as the AUR for Arch Linux as a source of third-party packages.

### MSYS2

[MSYS2](#msys2) is heavier than mingw-w64, but it comes with GNU `make`.

It offers a dedicated terminal environment with a more *NIX-like
approach than Powershell:

- Runs atop the Windows kernel
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
2. Extract the compiler to a folder you can add to `PATH`
3. [Add it to `PATH` variable](https://stackoverflow.com/questions/9546324/adding-a-directory-to-the-path-environment-variable-in-windows)

Afterward, you may need to restart or log out + in.


## Compiling

This section is at the bottom, but it's really not hard for our hand-picked dependencies.

### Compiling SDL3 and SDL3_ttf

The SDL team offers excellent documentation on compiling from source:

- [SDL3's `INSTALL.md`](https://github.com/libsdl-org/SDL/blob/main/INSTALL.md)
- [SDL3_ttf's `INSTALL.md`](https://github.com/libsdl-org/SDL_ttf/blob/main/INSTALL.md)

### Compiling `tcc`

1. Download [the source (`tcc-0.9.27.tar.bz2`)]() (via the [GNU download page](https://download.savannah.gnu.org/releases/tinycc/))
2. Compile with another C compiler (`clang` or `gcc` both work)

For usage info, please see [the `tcc` documentation](https://bellard.org/tcc/tcc-doc.html) for more information.
