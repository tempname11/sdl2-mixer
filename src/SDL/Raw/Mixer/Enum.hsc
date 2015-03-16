{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
module SDL.Raw.Mixer.Enum (
  pattern MIX_INIT_FLAC,
  pattern MIX_INIT_MOD,
  pattern MIX_INIT_MP3,
  pattern MIX_INIT_OGG,

  pattern AUDIO_U8,
  pattern AUDIO_S8,
  pattern AUDIO_U16LSB,
  pattern AUDIO_S16LSB,
  pattern AUDIO_U16MSB,
  pattern AUDIO_S16MSB,
  pattern AUDIO_U16,
  pattern AUDIO_S16,
  pattern AUDIO_U16SYS,
  pattern AUDIO_S16SYS,
  pattern MIX_DEFAULT_FORMAT,

  InitFlag,
  Format
) where

#include "SDL_Mixer.h"

import Foreign.C.Types
import Data.Word

type InitFlag = CInt
type Format = Word16

pattern MIX_INIT_FLAC = (#const MIX_INIT_FLAC) :: InitFlag
pattern MIX_INIT_MOD = (#const MIX_INIT_MOD) :: InitFlag
pattern MIX_INIT_MP3 = (#const MIX_INIT_MP3) :: InitFlag
pattern MIX_INIT_OGG = (#const MIX_INIT_OGG) :: InitFlag

pattern AUDIO_U8 = (#const AUDIO_U8) :: Format
pattern AUDIO_S8 = (#const AUDIO_S8) :: Format
pattern AUDIO_U16LSB = (#const AUDIO_U16LSB) :: Format
pattern AUDIO_S16LSB = (#const AUDIO_S16LSB) :: Format
pattern AUDIO_U16MSB = (#const AUDIO_U16MSB) :: Format
pattern AUDIO_S16MSB = (#const AUDIO_S16MSB) :: Format
pattern AUDIO_U16 = (#const AUDIO_U16) :: Format
pattern AUDIO_S16 = (#const AUDIO_S16) :: Format
pattern AUDIO_U16SYS = (#const AUDIO_U16SYS) :: Format
pattern AUDIO_S16SYS = (#const AUDIO_S16SYS) :: Format
pattern MIX_DEFAULT_FORMAT = (#const MIX_DEFAULT_FORMAT) :: Format
