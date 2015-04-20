-- A (hopefully) complete mapping of the C API.
-- See: https://www.libsdl.org/projects/SDL_mixer/docs/index.html
-- Mirror: http://jcatki.no-ip.org:8080/SDL_mixer/

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module SDL.Raw.Mixer
  (

  -- * General
    getVersion
  , pattern SDL_MIXER_MAJOR_VERSION
  , pattern SDL_MIXER_MINOR_VERSION
  , pattern SDL_MIXER_PATCHLEVEL
  , InitFlag
  , init
  , pattern MIX_INIT_FLAC
  , pattern MIX_INIT_MOD
  , pattern MIX_INIT_MP3
  , pattern MIX_INIT_OGG
  , quit
  , Format
  , openAudio
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
  , closeAudio
  , querySpec

  -- * Samples
  , getNumChunkDecoders
  , getChunkDecoder
  , Chunk(..)
  , loadWAV
  , loadWAV_RW
  , quickLoadWAV
  , quickLoadRaw
  , volumeChunk
  , freeChunk

  -- * Channels
  , allocateChannels
  , Channel
  , volume
  , playChannel
  , playChannelTimed
  , fadeInChannel
  , fadeInChannelTimed
  , pause
  , resume
  , haltChannel
  , expireChannel
  , fadeOutChannel
  , channelFinished
  , playing
  , paused
  , Fading
  , fadingChannel
  , getChunk

  -- * Groups
  , reserveChannels
  , Tag
  , groupChannel
  , groupChannels
  , groupCount
  , groupAvailable
  , groupOldest
  , groupNewer
  , fadeOutGroup
  , haltGroup

  -- * Music
  , getNumMusicDecoders
  , getMusicDecoder
  , loadMUS
  , freeMusic
  , playMusic
  , fadeInMusic
  , fadeInMusicPos
  , hookMusic
  , volumeMusic
  , pauseMusic
  , resumeMusic
  , rewindMusic
  , setMusicPosition
  , setMusicCMD
  , haltMusic
  , fadeOutMusic
  , hookMusicFinished
  , MusicType
  , getMusicType
  , playingMusic
  , pausedMusic
  , fadingMusic
  , getMusicHookData

  -- * Effects
  , registerEffect
  , unregisterEffect
  , unregisterAllEffects
  , setPostMix
  , setPanning
  , setDistance
  , setPosition
  , setReverseStereo

  ) where

#include "SDL_mixer.h"

import Data.Int         (Int16)
import Data.Word        (Word8, Word16, Word32)
import Foreign.C.String (CString)
import Foreign.C.Types  (CInt(..), CDouble(..))
import Foreign.Ptr      (Ptr, FunPtr)
import Foreign.Storable (Storable(..))
import Prelude   hiding (init)
import SDL.Raw.Helper   (liftF)
import SDL.Raw.Types    (RWops(..), Version(..))

-- 4.1 General

liftF "getVersion" "Mix_Linked_Version"
  [t|IO (Ptr Version)|]

pattern SDL_MIXER_MAJOR_VERSION = (#const SDL_MIXER_MAJOR_VERSION)
pattern SDL_MIXER_MINOR_VERSION = (#const SDL_MIXER_MINOR_VERSION)
pattern SDL_MIXER_PATCHLEVEL    = (#const SDL_MIXER_PATCHLEVEL)

type InitFlag = CInt

liftF "init" "Mix_Init"
  [t|InitFlag -> IO CInt|]

pattern MIX_INIT_FLAC = (#const MIX_INIT_FLAC)
pattern MIX_INIT_MOD  = (#const MIX_INIT_MOD)
pattern MIX_INIT_MP3  = (#const MIX_INIT_MP3)
pattern MIX_INIT_OGG  = (#const MIX_INIT_OGG)

liftF "quit" "Mix_Quit"
  [t|IO ()|]

type Format = Word16

liftF "openAudio" "Mix_OpenAudio"
  [t|CInt -> Format -> CInt -> CInt -> IO CInt|]

pattern AUDIO_U8           = (#const AUDIO_U8)
pattern AUDIO_S8           = (#const AUDIO_S8)
pattern AUDIO_U16LSB       = (#const AUDIO_U16LSB)
pattern AUDIO_S16LSB       = (#const AUDIO_S16LSB)
pattern AUDIO_U16MSB       = (#const AUDIO_U16MSB)
pattern AUDIO_S16MSB       = (#const AUDIO_S16MSB)
pattern AUDIO_U16          = (#const AUDIO_U16)
pattern AUDIO_S16          = (#const AUDIO_S16)
pattern AUDIO_U16SYS       = (#const AUDIO_U16SYS)
pattern AUDIO_S16SYS       = (#const AUDIO_S16SYS)
pattern MIX_DEFAULT_FORMAT = (#const MIX_DEFAULT_FORMAT)

liftF "closeAudio" "Mix_CloseAudio"
  [t|IO ()|]

liftF "querySpec" "Mix_QuerySpec"
  [t|Ptr CInt -> Ptr Format -> Ptr CInt -> IO CInt|]

-- 4.2 Samples

liftF "getNumChunkDecoders" "Mix_GetNumChunkDecoders"
  [t|IO CInt|]

liftF "getChunkDecoder" "Mix_GetChunkDecoder"
  [t|CInt -> IO CString|]

data Chunk = Chunk
  { chunkAllocated :: CInt
  , chunkAbuf      :: Ptr Word8
  , chunkAlen      :: Word32
  , chunkVolume    :: Word8
  } deriving (Eq, Show)

instance Storable Chunk where
  alignment = sizeOf
  sizeOf _  = (#size Mix_Chunk)

  peek ptr =
    Chunk
      <$> (#peek Mix_Chunk, allocated) ptr
      <*> (#peek Mix_Chunk, abuf)      ptr
      <*> (#peek Mix_Chunk, alen)      ptr
      <*> (#peek Mix_Chunk, volume)    ptr

  poke ptr (Chunk {..}) = do
    (#poke Mix_Chunk, allocated) ptr chunkAllocated
    (#poke Mix_Chunk, abuf)      ptr chunkAbuf
    (#poke Mix_Chunk, alen)      ptr chunkAlen
    (#poke Mix_Chunk, volume)    ptr chunkVolume

liftF "loadWAV" "Mix_LoadWAV"
  [t|CString -> IO (Ptr Chunk)|]

liftF "loadWAV_RW" "Mix_LoadWAV_RW"
  [t|Ptr RWops -> CInt -> IO (Ptr Chunk)|]

liftF "quickLoadWAV" "Mix_QuickLoad_WAV"
  [t|Ptr Word8 -> IO (Ptr Chunk)|]

liftF "quickLoadRaw" "Mix_QuickLoad_RAW"
  [t|Ptr Word8 -> IO (Ptr Chunk)|]

liftF "volumeChunk" "Mix_VolumeChunk"
  [t|Ptr Chunk -> CInt -> IO CInt|]

liftF "freeChunk" "Mix_FreeChunk"
  [t|Ptr Chunk -> IO ()|]

-- 4.3 Channels

liftF "allocateChannels" "Mix_AllocateChannels"
  [t|CInt -> IO CInt|]

type Channel = CInt

liftF "volume" "Mix_Volume"
  [t|Channel -> CInt -> IO CInt|]

liftF "playChannel" "Mix_PlayChannel"
  [t|Channel -> Ptr Chunk -> CInt -> IO CInt|]

liftF "playChannelTimed" "Mix_PlayChannelTimed"
  [t|Channel -> Ptr Chunk -> CInt -> CInt -> IO CInt|]

liftF "fadeInChannel" "Mix_FadeInChannel"
  [t|Channel -> Ptr Chunk -> CInt -> CInt -> IO CInt|]

liftF "fadeInChannelTimed" "Mix_FadeInChannelTimed"
  [t|Channel -> Ptr Chunk -> CInt -> CInt -> CInt -> IO CInt|]

liftF "pause" "Mix_Pause"
  [t|Channel -> IO ()|]

liftF "resume" "Mix_Resume"
  [t|Channel -> IO ()|]

liftF "haltChannel" "Mix_HaltChannel"
  [t|Channel -> IO CInt|]

liftF "expireChannel" "Mix_ExpireChannel"
  [t|Channel -> CInt -> IO CInt|]

liftF "fadeOutChannel" "Mix_FadeOutChannel"
  [t|Channel -> CInt -> IO CInt|]

liftF "channelFinished" "Mix_ChannelFinished"
  [t|FunPtr (Channel -> IO ()) -> IO ()|] -- FIXME: MonadIO, yikes!

liftF "playing" "Mix_Playing"
  [t|Channel -> IO CInt|]

liftF "paused" "Mix_Paused"
  [t|Channel -> IO CInt|]

type Fading = (#type Mix_Fading)

liftF "fadingChannel" "Mix_FadingChannel"
  [t|Channel -> IO Fading|]

liftF "getChunk" "Mix_GetChunk"
  [t|Channel -> IO (Ptr Chunk)|]

-- 4.4 Groups

liftF "reserveChannels" "Mix_ReserveChannels"
  [t|CInt -> IO CInt|]

type Tag = CInt

liftF "groupChannel" "Mix_GroupChannel"
  [t|Channel -> Tag -> IO CInt|]

liftF "groupChannels" "Mix_GroupChannels"
  [t|Channel -> Channel -> Tag -> IO CInt|]

liftF "groupCount" "Mix_GroupCount"
  [t|Tag -> IO CInt|]

liftF "groupAvailable" "Mix_GroupAvailable"
  [t|Tag -> IO CInt|]

liftF "groupOldest" "Mix_GroupOldest"
  [t|Tag -> IO CInt|]

liftF "groupNewer" "Mix_GroupNewer"
  [t|Tag -> IO CInt|]

liftF "fadeOutGroup" "Mix_FadeOutGroup"
  [t|Tag -> CInt -> IO CInt|]

liftF "haltGroup" "Mix_HaltGroup"
  [t|Tag -> IO CInt|]

-- 4.5 Music

liftF "getNumMusicDecoders" "Mix_GetNumMusicDecoders"
  [t|IO CInt|]

liftF "getMusicDecoder" "Mix_GetMusicDecoder"
  [t|CInt -> IO CString|]

data Music

liftF "loadMUS" "Mix_LoadMUS"
  [t|CString -> IO (Ptr Music)|]

liftF "freeMusic" "Mix_FreeMusic"
  [t|Ptr Music -> IO ()|]

liftF "playMusic" "Mix_PlayMusic"
  [t|Ptr Music -> CInt -> IO CInt|]

liftF "fadeInMusic" "Mix_FadeInMusic"
  [t|Ptr Music -> CInt -> CInt -> IO CInt|]

liftF "fadeInMusicPos" "Mix_FadeInMusicPos"
  [t|Ptr Music -> CInt -> CInt -> CDouble -> IO CInt|]

liftF "hookMusic" "Mix_HookMusic" -- TODO: MonadIO, yikes!
  [t|FunPtr (Ptr () -> Ptr Word8 -> CInt -> IO ()) -> Ptr () -> IO ()|]

liftF "volumeMusic" "Mix_VolumeMusic"
  [t|CInt -> IO CInt|]

liftF "pauseMusic" "Mix_PauseMusic"
  [t|IO ()|]

liftF "resumeMusic" "Mix_ResumeMusic"
  [t|IO ()|]

liftF "rewindMusic" "Mix_RewindMusic"
  [t|IO ()|]

liftF "setMusicPosition" "Mix_SetMusicPosition"
  [t|CDouble -> IO CInt|]

liftF "setMusicCMD" "Mix_SetMusicCMD"
  [t|CString -> IO CInt|]

liftF "haltMusic" "Mix_HaltMusic"
  [t|IO CInt|]

liftF "fadeOutMusic" "Mix_FadeOutMusic"
  [t|CInt -> IO CInt|]

liftF "hookMusicFinished" "Mix_HookMusicFinished"
  [t|FunPtr (IO ()) -> IO ()|] -- TODO: MonadIO, yikes!

type MusicType = (#type Mix_MusicType)

liftF "getMusicType" "Mix_GetMusicType"
  [t|Ptr Music -> IO MusicType|]

liftF "playingMusic" "Mix_PlayingMusic"
  [t|IO CInt|]

liftF "pausedMusic" "Mix_PausedMusic"
  [t|IO CInt|]

liftF "fadingMusic" "Mix_FadingChannel"
  [t|IO Fading|]

liftF "getMusicHookData" "Mix_GetMusicHookData"
  [t|IO (Ptr ())|]

-- 4.6 Effects

liftF "registerEffect" "Mix_RegisterEffect"
  [t|Channel ->
     FunPtr (Channel -> Ptr () -> CInt -> Ptr () -> IO ()) ->
     FunPtr (Channel -> Ptr () -> IO ()) ->
     Ptr () ->
     IO CInt|]

liftF "unregisterEffect" "Mix_UnregisterEffect"
  [t|Channel ->
     FunPtr (Channel -> Ptr () -> CInt -> Ptr () -> IO ()) ->
     IO CInt|]

liftF "unregisterAllEffects" "Mix_UnregisterAllEffects"
  [t|Channel -> IO CInt|]

liftF "setPostMix" "Mix_SetPostMix"
  [t|FunPtr (Ptr () -> Ptr Word8 -> CInt -> IO ()) -> Ptr () -> IO ()|]

liftF "setPanning" "Mix_SetPanning"
  [t|Channel -> Word8 -> Word8 -> IO CInt|]

liftF "setDistance" "Mix_SetDistance"
  [t|Channel -> Word8 -> IO CInt|]

liftF "setPosition" "Mix_SetPosition"
  [t|Channel -> Int16 -> Word8 -> IO CInt|]

liftF "setReverseStereo" "Mix_SetReverseStereo"
  [t|Channel -> CInt -> IO CInt|]
