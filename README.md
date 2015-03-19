# sdl2-mixer

This is a very early, incomplete version of Haskell bindings to [SDL_mixer](https://www.libsdl.org/projects/SDL_mixer/), version 2.0, intended for use with the [sdl2 package](https://github.com/haskell-game/sdl2), specifically the *new-api* branch. The goal for this library is for it to look like (and work seamlessly with) **sdl2**.

Only a small fraction of the API is exposed at the moment, but it's enough to read sound files from disk and play them at will, which is the most important thing, really.

I will be updating this library as I go along in making my own (undisclosed yet) game. If _(or rather, when)_ something you need is missing, feel free to add the functionality and make a pull request. It'd be greatly appreciated.

As the library matures a bit and the API becomes more or less fixed, I intend to upload this package to Hackage.

# Some misc. tips:

1. Take a look at [a basic usage example](examples/Basic.hs). I think it looks pretty clean.
2. In order for SDL_mixer to play MP3 files, you need the SMPEG library installed, and SDL_mixer to be configured with the support for it. On Mac OS X, this meant I couldn't use the sdl2_mixer package from [homebrew](http://brew.sh/), and had to install both libraries manually.
