# sdl2-mixer

[![Build Status](https://travis-ci.org/sbidin/sdl2-mixer.svg?branch=master)](https://travis-ci.org/sbidin/sdl2-mixer)

#### Haskell bindings to SDL2_mixer

Both the raw and the higher level bindings should allow you to use any aspect
of the original SDL2_mixer library. Please report an issue if you encounter a
bug or feel that something is missing.

##### Install

This library depends on the new API version of
[haskell-game/sdl2](https://github.com/haskell-game/sdl2), available on
Hackage as
[sdl2 version 2.0.0 or greater](http://hackage.haskell.org/package/sdl2). With
that in mind, you can install sdl2-mixer from source like this:

```bash
git clone git@github.com:sbidin/sdl2-mixer.git
cd sdl2-mixer
cabal install
```

Note that you might get compile errors if you're not using the latest GHC. Only
7.10 is currently tested.

##### Documentation

You can find the documentation [here](https://bidin.eu/docs/sdl2-mixer).

The
[original SDL2_mixer documentation](http://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html)
can also help, as the bindings are close to a direct mapping.

##### Example

Several example executables are included with the library. You can find them in
the `examples` directory.

```bash
cd sdl2-mixer
cabal run sdl2-mixer-basic <file>
cabal run sdl2-mixer-raw <file>
cabal run sdl2-mixer-music <file>
cabal run sdl2-mixer-jumbled <file1> ... <fileN>
cabal run sdl2-mixer-effect <file>
```

##### Miscellaneous tips

In order for `SDL2_mixer` to play MP3 files, you need the SMPEG library
installed, and `SDL2_mixer` to be configured with the support for it. On Mac OS
X, this means you can't use the `sdl2_mixer` package from
[homebrew](http://brew.sh/) and have to install both libraries manually.
