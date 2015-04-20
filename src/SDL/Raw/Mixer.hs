-- A (hopefully) complete mapping of the C API.
-- See: https://www.libsdl.org/projects/SDL_mixer/docs/index.html
-- Mirror: http://jcatki.no-ip.org:8080/SDL_mixer/

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

import Prelude hiding (init)
import Control.Monad.IO.Class
import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import SDL.Raw.Mixer.Enum
import SDL.Raw.Mixer.Types
import SDL.Raw.Types (RWops(..), Version(..))
import SDL.Raw.Filesystem (rwFromFile)

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
foreign import ccall "SDL_mixer.h Mix_Linked_Version" linkedVersion' :: IO (Ptr Version)
foreign import ccall "SDL_mixer.h Mix_Init" init' :: InitFlag -> IO CInt
foreign import ccall "SDL_mixer.h Mix_Quit" quit' :: IO ()
foreign import ccall "SDL_mixer.h Mix_OpenAudio" openAudio' :: CInt -> Format -> CInt -> CInt -> IO CInt
foreign import ccall "SDL_mixer.h Mix_CloseAudio" closeAudio' :: IO ()
foreign import ccall "SDL_mixer.h Mix_QuerySpec" querySpec' :: Ptr CInt -> Ptr Word16 -> Ptr CInt -> IO CInt

-- Samples
foreign import ccall "SDL_mixer.h Mix_GetNumChunkDecoders" getNumChunkDecoders' :: IO CInt
foreign import ccall "SDL_mixer.h Mix_GetChunkDecoder" getChunkDecoder' :: CInt -> IO CString
foreign import ccall "SDL_mixer.h Mix_LoadWAV_RW" loadWavRW' :: Ptr RWops -> CInt -> IO (Ptr Chunk)
foreign import ccall "SDL_mixer.h Mix_QuickLoad_WAV" quickLoadWav' :: Ptr Word8 -> IO (Ptr Chunk)
foreign import ccall "SDL_mixer.h Mix_QuickLoad_RAW" quickLoadRaw' :: Ptr Word8 -> Word32 -> IO (Ptr Chunk)
foreign import ccall "SDL_mixer.h Mix_VolumeChunk" volumeChunk' :: Ptr Chunk -> CInt -> IO CInt
foreign import ccall "SDL_mixer.h Mix_FreeChunk" freeChunk' :: Ptr Chunk -> IO ()

-- Channels
foreign import ccall "SDL_mixer.h Mix_AllocateChannels" allocateChannels' :: CInt -> IO CInt
foreign import ccall "SDL_mixer.h Mix_Volume" volume' :: Channel -> CInt -> IO CInt
foreign import ccall "SDL_mixer.h Mix_PlayChannelTimed" playChannelTimed' :: Channel -> Ptr Chunk -> CInt -> CInt -> IO CInt
foreign import ccall "SDL_mixer.h Mix_FadeInChannelTimed" fadeInChannelTimed' :: Channel -> Ptr Chunk -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall "SDL_mixer.h Mix_Pause" pause' :: Channel -> IO ()
foreign import ccall "SDL_mixer.h Mix_Resume" resume' :: Channel -> IO ()
foreign import ccall "SDL_mixer.h Mix_HaltChannel" haltChannel' :: Channel -> IO CInt
foreign import ccall "SDL_mixer.h Mix_ExpireChannel" expireChannel' :: Channel -> CInt -> IO CInt
foreign import ccall "SDL_mixer.h Mix_FadeOutChannel" fadeOutChannel' :: Channel -> CInt -> IO CInt
foreign import ccall "SDL_mixer.h Mix_ChannelFinished" channelFinished' :: ChannelFinishedCallback -> IO ()
foreign import ccall "SDL_mixer.h Mix_Playing" playing' :: Channel -> IO CInt
foreign import ccall "SDL_mixer.h Mix_Paused" paused' :: Channel -> IO CInt
foreign import ccall "SDL_mixer.h Mix_FadingChannel" fadingChannel' :: Channel -> IO Fading
foreign import ccall "SDL_mixer.h Mix_GetChunk" getChunk' :: Channel -> IO (Ptr Chunk)

-- Groups
foreign import ccall "SDL_mixer.h Mix_ReserveChannels" reserveChannels' :: CInt -> IO CInt
foreign import ccall "SDL_mixer.h Mix_GroupChannel" groupChannel' :: Channel -> Tag -> IO CInt
foreign import ccall "SDL_mixer.h Mix_GroupChannels" groupChannels' :: Channel -> Channel -> Tag -> IO CInt
foreign import ccall "SDL_mixer.h Mix_GroupCount" groupCount' :: Tag -> IO CInt
foreign import ccall "SDL_mixer.h Mix_GroupAvailable" groupAvailable' :: Tag -> IO CInt
foreign import ccall "SDL_mixer.h Mix_GroupOldest" groupOldest' :: Tag -> IO CInt
foreign import ccall "SDL_mixer.h Mix_GroupNewer" groupNewer' :: Tag -> IO CInt
foreign import ccall "SDL_mixer.h Mix_FadeOutGroup" fadeOutGroup' :: Tag -> CInt -> IO CInt
foreign import ccall "SDL_mixer.h Mix_HaltGroup" haltGroup' :: Tag -> IO CInt

-- Music
foreign import ccall "SDL_mixer.h Mix_GetNumMusicDecoders" getNumMusicDecoders' :: IO CInt
foreign import ccall "SDL_mixer.h Mix_GetMusicDecoder" getMusicDecoder' :: CInt -> IO CString
foreign import ccall "SDL_mixer.h Mix_LoadMUS" loadMus' :: CString -> IO (Ptr Music)
foreign import ccall "SDL_mixer.h Mix_FreeMusic" freeMusic' :: Ptr Music -> IO ()
foreign import ccall "SDL_mixer.h Mix_PlayMusic" playMusic' :: Ptr Music -> CInt -> IO CInt
foreign import ccall "SDL_mixer.h Mix_FadeInMusic" fadeInMusic' :: Ptr Music -> CInt -> CInt -> IO CInt
foreign import ccall "SDL_mixer.h Mix_FadeInMusicPos" fadeInMusicPos' :: Ptr Music -> CInt -> CInt -> CDouble -> IO CInt
foreign import ccall "SDL_mixer.h Mix_HookMusic" hookMusic' :: MixCallback -> Ptr () -> IO ()
foreign import ccall "SDL_mixer.h Mix_VolumeMusic" volumeMusic' :: CInt -> IO CInt
foreign import ccall "SDL_mixer.h Mix_PauseMusic" pauseMusic' :: IO ()
foreign import ccall "SDL_mixer.h Mix_ResumeMusic" resumeMusic' :: IO ()
foreign import ccall "SDL_mixer.h Mix_RewindMusic" rewindMusic' :: IO ()
foreign import ccall "SDL_mixer.h Mix_SetMusicPosition" setMusicPosition' :: CDouble -> IO CInt
foreign import ccall "SDL_mixer.h Mix_SetMusicCMD" setMusicCmd' :: CString -> IO CInt
foreign import ccall "SDL_mixer.h Mix_HaltMusic" haltMusic' :: IO CInt
foreign import ccall "SDL_mixer.h Mix_FadeOutMusic" fadeOutMusic' :: CInt -> IO CInt
foreign import ccall "SDL_mixer.h Mix_HookMusicFinished" hookMusicFinished' :: MusicFinishedCallback -> IO ()
foreign import ccall "SDL_mixer.h Mix_GetMusicType" getMusicType' :: Ptr Music -> IO MusicType
foreign import ccall "SDL_mixer.h Mix_PlayingMusic" playingMusic' :: IO CInt
foreign import ccall "SDL_mixer.h Mix_PausedMusic" pausedMusic' :: IO CInt
foreign import ccall "SDL_mixer.h Mix_FadingChannel" fadingMusic' :: IO Fading
foreign import ccall "SDL_mixer.h Mix_GetMusicHookData" getMusicHookData' :: IO (Ptr ())

-- Effects
foreign import ccall "SDL_mixer.h Mix_RegisterEffect" registerEffect' :: Channel -> EffectCallback -> EffectDoneCallback -> Ptr () -> IO CInt
foreign import ccall "SDL_mixer.h Mix_UnregisterEffect" unregisterEffect' :: Channel -> EffectCallback -> IO CInt
foreign import ccall "SDL_mixer.h Mix_UnregisterAllEffects" unregisterAllEffects' :: Channel -> IO CInt
foreign import ccall "SDL_mixer.h Mix_SetPostMix" setPostMix' :: MixCallback -> Ptr () -> IO ()
foreign import ccall "SDL_mixer.h Mix_SetPanning" setPanning' :: Channel -> Word8 -> Word8 -> IO CInt
foreign import ccall "SDL_mixer.h Mix_SetDistance" setDistance' :: Channel -> Word8 -> IO CInt
foreign import ccall "SDL_mixer.h Mix_SetPosition" setPosition' :: Channel -> Int16 -> Word8 -> IO CInt
foreign import ccall "SDL_mixer.h Mix_SetReverseStereo" setReverseStereo' :: Channel -> CInt -> IO CInt

-- General --

compiledVersion :: Version
compiledVersion = Version major minor patch
  where
    major = SDL_MIXER_MAJOR_VERSION
    minor = SDL_MIXER_MINOR_VERSION
    patch = SDL_MIXER_PATCHLEVEL

linkedVersion :: MonadIO m => m (Ptr Version)
linkedVersion = liftIO linkedVersion'
{-# INLINE linkedVersion #-}

init :: MonadIO m => InitFlag -> m CInt
init v1 = liftIO $ init' v1
{-# INLINE init #-}

quit :: MonadIO m => m ()
quit = liftIO quit'
{-# INLINE quit #-}

openAudio :: MonadIO m => CInt -> Format -> CInt -> CInt -> m CInt
openAudio v1 v2 v3 v4 = liftIO $ openAudio' v1 v2 v3 v4
{-# INLINE openAudio #-}

closeAudio :: MonadIO m => m ()
closeAudio = liftIO closeAudio'
{-# INLINE closeAudio #-}

querySpec :: MonadIO m => Ptr CInt -> Ptr Word16 -> Ptr CInt -> m CInt
querySpec v1 v2 v3 = liftIO $ querySpec' v1 v2 v3
{-# INLINE querySpec #-}

-- Samples --

getNumChunkDecoders :: MonadIO m => m CInt
getNumChunkDecoders = liftIO getNumChunkDecoders'
{-# INLINE getNumChunkDecoders #-}

getChunkDecoder :: MonadIO m => CInt -> m CString
getChunkDecoder v1 = liftIO $ getChunkDecoder' v1
{-# INLINE getChunkDecoder #-}

loadWav :: MonadIO m => CString -> m (Ptr Chunk)
loadWav v1 = liftIO $ withCString "rb" $ \rb -> do
  file <- rwFromFile v1 rb
  loadWavRW' (file) 1
{-# INLINE loadWav #-}

loadWavRW :: MonadIO m => Ptr RWops -> CInt -> m (Ptr Chunk)
loadWavRW v1 v2 = liftIO $ loadWavRW' v1 v2
{-# INLINE loadWavRW #-}

quickLoadWav :: MonadIO m => Ptr Word8 -> m (Ptr Chunk)
quickLoadWav v1 = liftIO $ quickLoadWav' v1
{-# INLINE quickLoadWav #-}

quickLoadRaw :: MonadIO m => Ptr Word8 -> Word32 -> m (Ptr Chunk)
quickLoadRaw v1 v2 = liftIO $ quickLoadRaw' v1 v2
{-# INLINE quickLoadRaw #-}

volumeChunk :: MonadIO m => Ptr Chunk -> CInt -> m CInt
volumeChunk v1 v2 = liftIO $ volumeChunk' v1 v2
{-# INLINE volumeChunk #-}

freeChunk :: MonadIO m => Ptr Chunk -> m ()
freeChunk v1 = liftIO $ freeChunk' v1
{-# INLINE freeChunk #-}

-- Channels --

allocateChannels :: MonadIO m => CInt -> m CInt
allocateChannels v1 = liftIO $ allocateChannels' v1
{-# INLINE allocateChannels #-}

volume :: MonadIO m => Channel -> CInt -> m CInt
volume v1 v2 = liftIO $ volume' v1 v2
{-# INLINE volume #-}

playChannel :: MonadIO m => Channel -> Ptr Chunk -> CInt -> m CInt
playChannel v1 v2 v3 = liftIO $ playChannelTimed' v1 v2 v3 (-1)
{-# INLINE playChannel #-}

playChannelTimed :: MonadIO m => Channel -> Ptr Chunk -> CInt -> CInt -> m CInt
playChannelTimed v1 v2 v3 v4 = liftIO $ playChannelTimed' v1 v2 v3 v4
{-# INLINE playChannelTimed #-}

fadeInChannel :: MonadIO m => Channel -> Ptr Chunk -> CInt -> CInt -> m CInt
fadeInChannel v1 v2 v3 v4 = liftIO $ fadeInChannelTimed' v1 v2 v3 v4 (-1)
{-# INLINE fadeInChannel #-}

fadeInChannelTimed :: MonadIO m => Channel -> Ptr Chunk -> CInt -> CInt -> CInt -> m CInt
fadeInChannelTimed v1 v2 v3 v4 v5 = liftIO $ fadeInChannelTimed' v1 v2 v3 v4 v5
{-# INLINE fadeInChannelTimed #-}

pause :: MonadIO m => Channel -> m ()
pause v1 = liftIO $ pause' v1
{-# INLINE pause #-}

resume :: MonadIO m => Channel -> m ()
resume v1 = liftIO $ resume' v1
{-# INLINE resume #-}

haltChannel :: MonadIO m => Channel -> m CInt
haltChannel v1 = liftIO $ haltChannel' v1
{-# INLINE haltChannel #-}

expireChannel :: MonadIO m => Channel -> CInt -> m CInt
expireChannel v1 v2 = liftIO $ expireChannel' v1 v2
{-# INLINE expireChannel #-}

fadeOutChannel :: MonadIO m => Channel -> CInt -> m CInt
fadeOutChannel v1 v2 = liftIO $ fadeOutChannel' v1 v2
{-# INLINE fadeOutChannel #-}

channelFinished :: MonadIO m => FunPtr (Channel -> IO ()) -> m ()
channelFinished v1 = liftIO $ channelFinished' v1
{-# INLINE channelFinished #-}

playing :: MonadIO m => Channel -> m CInt
playing v1 = liftIO $ playing' v1
{-# INLINE playing #-}

paused :: MonadIO m => Channel -> m CInt
paused v1 = liftIO $ paused' v1
{-# INLINE paused #-}

fadingChannel :: MonadIO m => Channel -> m Fading
fadingChannel v1 = liftIO $ fadingChannel' v1
{-# INLINE fadingChannel #-}

getChunk :: MonadIO m => Channel -> m (Ptr Chunk)
getChunk v1 = liftIO $ getChunk' v1
{-# INLINE getChunk #-}

-- Groups --

reserveChannels :: MonadIO m => CInt -> m CInt
reserveChannels v1 = liftIO $ reserveChannels' v1
{-# INLINE reserveChannels #-}

groupChannel :: MonadIO m => Channel -> Tag -> m CInt
groupChannel v1 v2 = liftIO $ groupChannel' v1 v2
{-# INLINE groupChannel #-}

groupChannels :: MonadIO m => Channel -> Channel -> Tag -> m CInt
groupChannels v1 v2 v3 = liftIO $ groupChannels' v1 v2 v3
{-# INLINE groupChannels #-}

groupCount :: MonadIO m => Tag -> m CInt
groupCount v1 = liftIO $ groupCount' v1
{-# INLINE groupCount #-}

groupAvailable :: MonadIO m => Tag -> m CInt
groupAvailable v1 = liftIO $ groupAvailable' v1
{-# INLINE groupAvailable #-}

groupOldest :: MonadIO m => Tag -> m CInt
groupOldest v1 = liftIO $ groupOldest' v1
{-# INLINE groupOldest #-}

groupNewer :: MonadIO m => Tag -> m CInt
groupNewer v1 = liftIO $ groupNewer' v1
{-# INLINE groupNewer #-}

fadeOutGroup :: MonadIO m => Tag -> CInt -> m CInt
fadeOutGroup v1 v2 = liftIO $ fadeOutGroup' v1 v2
{-# INLINE fadeOutGroup #-}

haltGroup :: MonadIO m => Tag -> m CInt
haltGroup v1 = liftIO $ haltGroup' v1
{-# INLINE haltGroup #-}

-- Music --

getNumMusicDecoders :: MonadIO m => m CInt
getNumMusicDecoders = liftIO getNumMusicDecoders'
{-# INLINE getNumMusicDecoders #-}

getMusicDecoder :: MonadIO m => CInt -> m CString
getMusicDecoder v1 = liftIO $ getMusicDecoder' v1
{-# INLINE getMusicDecoder #-}

loadMus :: MonadIO m => CString -> m (Ptr Music)
loadMus v1 = liftIO $ loadMus' v1
{-# INLINE loadMus #-}

freeMusic :: MonadIO m => Ptr Music -> m ()
freeMusic v1 = liftIO $ freeMusic' v1
{-# INLINE freeMusic #-}

playMusic :: MonadIO m => Ptr Music -> CInt -> m CInt
playMusic v1 v2 = liftIO $ playMusic' v1 v2
{-# INLINE playMusic #-}

fadeInMusic :: MonadIO m => Ptr Music -> CInt -> CInt -> m CInt
fadeInMusic v1 v2 v3 = liftIO $ fadeInMusic' v1 v2 v3
{-# INLINE fadeInMusic #-}

fadeInMusicPos :: MonadIO m => Ptr Music -> CInt -> CInt -> CDouble -> m CInt
fadeInMusicPos v1 v2 v3 v4 = liftIO $ fadeInMusicPos' v1 v2 v3 v4
{-# INLINE fadeInMusicPos #-}

hookMusic :: MonadIO m => MixCallback -> Ptr () -> m ()
hookMusic v1 v2 = liftIO $ hookMusic' v1 v2
{-# INLINE hookMusic #-}

volumeMusic :: MonadIO m => CInt -> m CInt
volumeMusic v1 = liftIO $ volumeMusic' v1
{-# INLINE volumeMusic #-}

pauseMusic :: MonadIO m => m ()
pauseMusic = liftIO pauseMusic'
{-# INLINE pauseMusic #-}

resumeMusic :: MonadIO m => m ()
resumeMusic = liftIO resumeMusic'
{-# INLINE resumeMusic #-}

rewindMusic :: MonadIO m => m ()
rewindMusic = liftIO rewindMusic'
{-# INLINE rewindMusic #-}

setMusicPosition :: MonadIO m => CDouble -> m CInt
setMusicPosition v1 = liftIO $ setMusicPosition' v1
{-# INLINE setMusicPosition #-}

setMusicCmd :: MonadIO m => CString -> m CInt
setMusicCmd v1 = liftIO $ setMusicCmd' v1
{-# INLINE setMusicCmd #-}

haltMusic :: MonadIO m => m CInt
haltMusic = liftIO haltMusic'
{-# INLINE haltMusic #-}

fadeOutMusic :: MonadIO m => CInt -> m CInt
fadeOutMusic v1 = liftIO $ fadeOutMusic' v1
{-# INLINE fadeOutMusic #-}

hookMusicFinished :: MonadIO m => MusicFinishedCallback -> m ()
hookMusicFinished v1 = liftIO $ hookMusicFinished' v1
{-# INLINE hookMusicFinished #-}

getMusicType :: MonadIO m => Ptr Music -> m MusicType
getMusicType v1 = liftIO $ getMusicType' v1
{-# INLINE getMusicType #-}

playingMusic :: MonadIO m => m CInt
playingMusic = liftIO playingMusic'
{-# INLINE playingMusic #-}

pausedMusic :: MonadIO m => m CInt
pausedMusic = liftIO pausedMusic'
{-# INLINE pausedMusic #-}

fadingMusic :: MonadIO m => m Fading
fadingMusic = liftIO fadingMusic'
{-# INLINE fadingMusic #-}

getMusicHookData :: MonadIO m => m (Ptr ())
getMusicHookData = liftIO getMusicHookData'
{-# INLINE getMusicHookData #-}

-- Effects --

registerEffect :: MonadIO m => Channel -> EffectCallback -> EffectDoneCallback -> Ptr () -> m CInt
registerEffect v1 v2 v3 v4 = liftIO $ registerEffect' v1 v2 v3 v4
{-# INLINE registerEffect #-}

unregisterEffect :: MonadIO m => Channel -> EffectCallback -> m CInt
unregisterEffect v1 v2 = liftIO $ unregisterEffect' v1 v2
{-# INLINE unregisterEffect #-}

unregisterAllEffects :: MonadIO m => Channel -> m CInt
unregisterAllEffects v1 = liftIO $ unregisterAllEffects' v1
{-# INLINE unregisterAllEffects #-}

setPostMix :: MonadIO m => MixCallback -> Ptr () -> m ()
setPostMix v1 v2 = liftIO $ setPostMix' v1 v2
{-# INLINE setPostMix #-}

setPanning :: MonadIO m => Channel -> Word8 -> Word8 -> m CInt
setPanning v1 v2 v3 = liftIO $ setPanning' v1 v2 v3
{-# INLINE setPanning #-}

setDistance :: MonadIO m => Channel -> Word8 -> m CInt
setDistance v1 v2 = liftIO $ setDistance' v1 v2
{-# INLINE setDistance #-}

setPosition :: MonadIO m => Channel -> Int16 -> Word8 -> m CInt
setPosition v1 v2 v3 = liftIO $ setPosition' v1 v2 v3
{-# INLINE setPosition #-}

setReverseStereo :: MonadIO m => Channel -> CInt -> m CInt
setReverseStereo v1 v2 = liftIO $ setReverseStereo' v1 v2
{-# INLINE setReverseStereo #-}
