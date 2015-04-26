# sdl2-mixer

[![Build Status](https://travis-ci.org/sbidin/sdl2-mixer.svg?branch=master)](https://travis-ci.org/sbidin/sdl2-mixer)

#### Haskell bindings to SDL2_mixer

The raw bindings are complete, but the higher-level ones are still missing the
following functionality:

* _using external or custom music players_,
* _effects_, and
* _sound font support_.

Please report an issue if you encounter a bug or feel something is missing.

##### Install

This library depends on and is meant to be used with the `new-api` branch of
[haskell-game/sdl2](https://github.com/haskell-game/sdl2). After installing
haskell-game/sdl2, you can install sdl2-mixer manually from source:

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
```

##### Miscellaneous tips

In order for `SDL2_mixer` to play MP3 files, you need the SMPEG library
installed, and `SDL2_mixer` to be configured with the support for it. On Mac OS
X, this means you can't use the `sdl2_mixer` package from
[homebrew](http://brew.sh/) and have to install both libraries manually.
