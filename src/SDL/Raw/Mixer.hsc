-- A (hopefully) complete mapping of the C API.
-- See: https://www.libsdl.org/projects/SDL_mixer/docs/index.html
-- Mirror: http://jcatki.no-ip.org:8080/SDL_mixer/

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module SDL.Raw.Mixer where
  -- -- General
  -- compiledVersion,
  -- linkedVersion,
  -- init,
  -- quit,
  -- openAudio,
  -- closeAudio,
  -- querySpec,

  -- -- Samples
  -- getNumChunkDecoders,
  -- getChunkDecoder,
  -- loadWav,
  -- loadWavRW,
  -- quickLoadWav,
  -- quickLoadRaw,
  -- volumeChunk,
  -- freeChunk,

  -- -- Channels
  -- allocateChannels,
  -- volume,
  -- playChannel,
  -- playChannelTimed,
  -- fadeInChannel,
  -- fadeInChannelTimed,
  -- pause,
  -- resume,
  -- haltChannel,
  -- expireChannel,
  -- fadeOutChannel,
  -- channelFinished,
  -- playing,
  -- paused,
  -- fadingChannel,
  -- getChunk,

  -- -- Groups
  -- reserveChannels,
  -- groupChannel,
  -- groupChannels,
  -- groupCount,
  -- groupAvailable,
  -- groupOldest,
  -- groupNewer,
  -- fadeOutGroup,
  -- haltGroup,

  -- -- Music
  -- getNumMusicDecoders,
  -- getMusicDecoder,
  -- loadMus,
  -- freeMusic,
  -- playMusic,
  -- fadeInMusic,
  -- fadeInMusicPos,
  -- hookMusic,
  -- volumeMusic,
  -- pauseMusic,
  -- resumeMusic,
  -- rewindMusic,
  -- setMusicPosition,
  -- setMusicCmd,
  -- haltMusic,
  -- fadeOutMusic,
  -- hookMusicFinished,
  -- getMusicType,
  -- playingMusic,
  -- pausedMusic,
  -- fadingMusic,
  -- getMusicHookData,

  -- -- Effects
  -- registerEffect,
  -- unregisterEffect,
  -- unregisterAllEffects,
  -- setPostMix,
  -- setPanning,
  -- setDistance,
  -- setPosition,
  -- setReverseStereo,

#include "SDL_mixer.h"

import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable (Storable(..))
import Prelude hiding (init)
-- import SDL.Raw.Filesystem (rwFromFile)
import SDL.Raw.Helper (liftF)
import SDL.Raw.Types (RWops(..), Version(..))

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

type InitFlag = CInt
type Format = Word16
type Tag = CInt
type Channel = CInt
type Fading = (#type Mix_Fading)
type ChannelFinishedCallback = FunPtr (Channel -> IO ())
type EffectCallback = FunPtr (Channel -> Ptr () -> CInt -> Ptr () -> IO ())
type EffectDoneCallback = FunPtr (Channel -> Ptr () -> IO ())
type MixCallback = FunPtr (Ptr () -> Ptr Word8 -> CInt -> IO ())
type MusicFinishedCallback = FunPtr (IO ())
type MusicType = (#type Mix_MusicType)

data Music

data Chunk = Chunk
  { chunkAllocated :: CInt
  , chunkAbuf      :: Ptr Word8
  , chunkAlen      :: Word32
  , chunkVolume    :: Word8
  } deriving (Eq, Show)

instance Storable Chunk where
  alignment = sizeOf
  sizeOf _  = (#size Mix_Chunk)

  peek ptr = do
    allocated <- (#peek Mix_Chunk, allocated) ptr
    abuf      <- (#peek Mix_Chunk, abuf) ptr
    alen      <- (#peek Mix_Chunk, alen) ptr
    volume    <- (#peek Mix_Chunk, volume) ptr
    return $! Chunk allocated abuf alen volume

  poke ptr (Chunk {..}) = do
    (#poke Mix_Chunk, allocated) ptr chunkAllocated
    (#poke Mix_Chunk, abuf)      ptr chunkAbuf
    (#poke Mix_Chunk, alen)      ptr chunkAlen
    (#poke Mix_Chunk, volume)    ptr chunkVolume

-- General
liftF "getVersion" "Mix_Linked_Version" [t|IO (Ptr Version)|]
liftF "init" "Mix_Init" [t|InitFlag -> IO CInt|]
liftF "quit" "Mix_Quit" [t|IO ()|]
liftF "openAudio" "Mix_OpenAudio" [t|CInt -> Format -> CInt -> CInt -> IO CInt|]
liftF "closeAudio" "Mix_CloseAudio" [t|IO ()|]
liftF "querySpec" "Mix_QuerySpec" [t|Ptr CInt -> Ptr Word16 -> Ptr CInt -> IO CInt|]

-- Samples
liftF "getNumChunkDecoders" "Mix_GetNumChunkDecoders" [t|IO CInt|]
liftF "getChunkDecoder" "Mix_GetChunkDecoder" [t|CInt -> IO CString|]
liftF "loadWavRW" "Mix_LoadWAV_RW" [t|Ptr RWops -> CInt -> IO (Ptr Chunk)|]
liftF "quickLoadWav" "Mix_QuickLoad_WAV" [t|Ptr Word8 -> IO (Ptr Chunk)|]
liftF "quickLoadRaw" "Mix_QuickLoad_RAW" [t|Ptr Word8 -> Word32 -> IO (Ptr Chunk)|]
liftF "volumeChunk" "Mix_VolumeChunk" [t|Ptr Chunk -> CInt -> IO CInt|]
liftF "freeChunk" "Mix_FreeChunk" [t|Ptr Chunk -> IO ()|]

-- Channels
liftF "allocateChannels" "Mix_AllocateChannels" [t|CInt -> IO CInt|]
liftF "volume" "Mix_Volume" [t|Channel -> CInt -> IO CInt|]
liftF "playChannelTimed" "Mix_PlayChannelTimed" [t|Channel -> Ptr Chunk -> CInt -> CInt -> IO CInt|]
liftF "fadeInChannelTimed" "Mix_FadeInChannelTimed" [t|Channel -> Ptr Chunk -> CInt -> CInt -> CInt -> IO CInt|]
liftF "pause" "Mix_Pause" [t|Channel -> IO ()|]
liftF "resume" "Mix_Resume" [t|Channel -> IO ()|]
liftF "haltChannel" "Mix_HaltChannel" [t|Channel -> IO CInt|]
liftF "expireChannel" "Mix_ExpireChannel" [t|Channel -> CInt -> IO CInt|]
liftF "fadeOutChannel" "Mix_FadeOutChannel" [t|Channel -> CInt -> IO CInt|]
liftF "channelFinished" "Mix_ChannelFinished" [t|ChannelFinishedCallback -> IO ()|]
liftF "playing" "Mix_Playing" [t|Channel -> IO CInt|]
liftF "paused" "Mix_Paused" [t|Channel -> IO CInt|]
liftF "fadingChannel" "Mix_FadingChannel" [t|Channel -> IO Fading|]
liftF "getChunk" "Mix_GetChunk" [t|Channel -> IO (Ptr Chunk)|]

-- Groups
liftF "reserveChannels" "Mix_ReserveChannels" [t|CInt -> IO CInt|]
liftF "groupChannel" "Mix_GroupChannel" [t|Channel -> Tag -> IO CInt|]
liftF "groupChannels" "Mix_GroupChannels" [t|Channel -> Channel -> Tag -> IO CInt|]
liftF "groupCount" "Mix_GroupCount" [t|Tag -> IO CInt|]
liftF "groupAvailable" "Mix_GroupAvailable" [t|Tag -> IO CInt|]
liftF "groupOldest" "Mix_GroupOldest" [t|Tag -> IO CInt|]
liftF "groupNewer" "Mix_GroupNewer" [t|Tag -> IO CInt|]
liftF "fadeOutGroup" "Mix_FadeOutGroup" [t|Tag -> CInt -> IO CInt|]
liftF "haltGroup" "Mix_HaltGroup" [t|Tag -> IO CInt|]

-- Music
liftF "getNumMusicDecoders" "Mix_GetNumMusicDecoders" [t|IO CInt|]
liftF "getMusicDecoder" "Mix_GetMusicDecoder" [t|CInt -> IO CString|]
liftF "loadMus" "Mix_LoadMUS" [t|CString -> IO (Ptr Music)|]
liftF "freeMusic" "Mix_FreeMusic" [t|Ptr Music -> IO ()|]
liftF "playMusic" "Mix_PlayMusic" [t|Ptr Music -> CInt -> IO CInt|]
liftF "fadeInMusic" "Mix_FadeInMusic" [t|Ptr Music -> CInt -> CInt -> IO CInt|]
liftF "fadeInMusicPos" "Mix_FadeInMusicPos" [t|Ptr Music -> CInt -> CInt -> CDouble -> IO CInt|]
liftF "hookMusic" "Mix_HookMusic" [t|MixCallback -> Ptr () -> IO ()|]
liftF "volumeMusic" "Mix_VolumeMusic" [t|CInt -> IO CInt|]
liftF "pauseMusic" "Mix_PauseMusic" [t|IO ()|]
liftF "resumeMusic" "Mix_ResumeMusic" [t|IO ()|]
liftF "rewindMusic" "Mix_RewindMusic" [t|IO ()|]
liftF "setMusicPosition" "Mix_SetMusicPosition" [t|CDouble -> IO CInt|]
liftF "setMusicCmd" "Mix_SetMusicCMD" [t|CString -> IO CInt|]
liftF "haltMusic" "Mix_HaltMusic" [t|IO CInt|]
liftF "fadeOutMusic" "Mix_FadeOutMusic" [t|CInt -> IO CInt|]
liftF "hookMusicFinished" "Mix_HookMusicFinished" [t|MusicFinishedCallback -> IO ()|]
liftF "getMusicType" "Mix_GetMusicType" [t|Ptr Music -> IO MusicType|]
liftF "playingMusic" "Mix_PlayingMusic" [t|IO CInt|]
liftF "pausedMusic" "Mix_PausedMusic" [t|IO CInt|]
liftF "fadingMusic" "Mix_FadingChannel" [t|IO Fading|]
liftF "getMusicHookData" "Mix_GetMusicHookData" [t|IO (Ptr ())|]

-- Effects
liftF "registerEffect" "Mix_RegisterEffect" [t|Channel -> EffectCallback -> EffectDoneCallback -> Ptr () -> IO CInt|]
liftF "unregisterEffect" "Mix_UnregisterEffect" [t|Channel -> EffectCallback -> IO CInt|]
liftF "unregisterAllEffects" "Mix_UnregisterAllEffects" [t|Channel -> IO CInt|]
liftF "setPostMix" "Mix_SetPostMix" [t|MixCallback -> Ptr () -> IO ()|]
liftF "setPanning" "Mix_SetPanning" [t|Channel -> Word8 -> Word8 -> IO CInt|]
liftF "setDistance" "Mix_SetDistance" [t|Channel -> Word8 -> IO CInt|]
liftF "setPosition" "Mix_SetPosition" [t|Channel -> Int16 -> Word8 -> IO CInt|]
liftF "setReverseStereo" "Mix_SetReverseStereo" [t|Channel -> CInt -> IO CInt|]

-- loadWav :: MonadIO m => CString -> m (Ptr Chunk)
-- loadWav v1 = liftIO $ withCString "rb" $ \rb -> do
--   file <- rwFromFile v1 rb
--   loadWavRW (file) 1

-- playChannel :: MonadIO m => Channel -> Ptr Chunk -> CInt -> m CInt
-- playChannel v1 v2 v3 = liftIO $ playChannelTimed' v1 v2 v3 (-1)
