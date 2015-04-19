{-# LANGUAGE PatternSynonyms #-}

module SDL.Raw.Mixer.Enum
  ( pattern MIX_INIT_FLAC
  , pattern MIX_INIT_MOD
  , pattern MIX_INIT_MP3
  , pattern MIX_INIT_OGG
  , pattern AUDIO_U8
  , pattern AUDIO_S8
  , pattern AUDIO_U16LSB
  , pattern AUDIO_S16LSB
  , pattern AUDIO_U16MSB
  , pattern AUDIO_S16MSB
  , pattern AUDIO_U16
  , pattern AUDIO_S16
  , pattern AUDIO_U16SYS
  , pattern AUDIO_S16SYS
  , pattern MIX_DEFAULT_FORMAT
  , pattern SDL_MIXER_MAJOR_VERSION
  , pattern SDL_MIXER_MINOR_VERSION
  , pattern SDL_MIXER_PATCHLEVEL
  ) where

#include "SDL_mixer.h"

pattern MIX_INIT_FLAC = (#const MIX_INIT_FLAC)
pattern MIX_INIT_MOD  = (#const MIX_INIT_MOD)
pattern MIX_INIT_MP3  = (#const MIX_INIT_MP3)
pattern MIX_INIT_OGG  = (#const MIX_INIT_OGG)

pattern AUDIO_U8     = (#const AUDIO_U8)
pattern AUDIO_S8     = (#const AUDIO_S8)
pattern AUDIO_U16LSB = (#const AUDIO_U16LSB)
pattern AUDIO_S16LSB = (#const AUDIO_S16LSB)
pattern AUDIO_U16MSB = (#const AUDIO_U16MSB)
pattern AUDIO_S16MSB = (#const AUDIO_S16MSB)
pattern AUDIO_U16    = (#const AUDIO_U16)
pattern AUDIO_S16    = (#const AUDIO_S16)
pattern AUDIO_U16SYS = (#const AUDIO_U16SYS)
pattern AUDIO_S16SYS = (#const AUDIO_S16SYS)

pattern MIX_DEFAULT_FORMAT = (#const MIX_DEFAULT_FORMAT)

pattern SDL_MIXER_MAJOR_VERSION = (#const SDL_MIXER_MAJOR_VERSION)
pattern SDL_MIXER_MINOR_VERSION = (#const SDL_MIXER_MINOR_VERSION)
pattern SDL_MIXER_PATCHLEVEL    = (#const SDL_MIXER_PATCHLEVEL)
