{-|

Module      : SDL.Mixer
License     : BSD3
Stability   : experimental

Bindings to the @SDL2_mixer@ library.

-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module SDL.Mixer
  (
  -- * Initialization
    initialize
  , InitFlag(..)
  , quit
  , version

  -- * Configure an audio device
  , openAudio
  , Audio(..)
  , ChunkSize
  , Format(..)
  , Output(..)
  , queryAudio
  , closeAudio

  ) where

import Control.Monad          (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits              ((.|.), (.&.))
import Data.Default.Class     (Default(def))
import Data.Foldable          (foldl)
import Foreign.C.Types        (CInt)
import Foreign.Marshal.Alloc  (alloca)
import Foreign.Storable       (Storable(..))
import Prelude         hiding (foldl)
import SDL.Exception          (throwIfNeg_, throwIf_, throwIf0)

import qualified SDL.Raw
import qualified SDL.Raw.Mixer

-- | Gets the major, minor, patch versions of the linked @SDL2_mixer@ library.
version :: (Integral a, MonadIO m) => m (a, a, a)
version = liftIO $ do
  SDL.Raw.Version major minor patch <- peek =<< SDL.Raw.Mixer.getVersion
  return (fromIntegral major, fromIntegral minor, fromIntegral patch)

-- | Initialize the library by loading support for a certain set of
-- sample/music formats. You may call this function multiple times. Note that
-- calling this is not strictly necessary: support for a certain format will be
-- loaded automatically when attempting to load data in that format. Using
-- 'initialize' allows you to decide /when/ to load support.
initialize :: (Foldable f, Functor m, MonadIO m) => f InitFlag -> m ()
initialize flags = do
  let raw = foldl (\a b -> a .|. initToCInt b) 0 flags
  throwIf_ ((/= raw) . (.&. raw)) "SDL.Mixer.initialize" "Mix_Init" $
    SDL.Raw.Mixer.init raw

-- | Used with 'initialize' to designate loading support for a particular
-- sample/music format.
data InitFlag
  = InitFLAC
  | InitMOD
  | InitMODPlug
  | InitMP3
  | InitOGG
  | InitFluidSynth
  deriving (Eq, Ord, Bounded, Read, Show)

initToCInt :: InitFlag -> CInt
initToCInt = \case
  InitFLAC       -> SDL.Raw.Mixer.MIX_INIT_FLAC
  InitMOD        -> SDL.Raw.Mixer.MIX_INIT_MOD
  InitMODPlug    -> SDL.Raw.Mixer.MIX_INIT_MODPLUG
  InitMP3        -> SDL.Raw.Mixer.MIX_INIT_MP3
  InitOGG        -> SDL.Raw.Mixer.MIX_INIT_OGG
  InitFluidSynth -> SDL.Raw.Mixer.MIX_INIT_FLUIDSYNTH

-- | Cleans up any loaded libraries, freeing memory.
quit :: MonadIO m => m ()
quit = SDL.Raw.Mixer.quit -- FIXME: May not free all init'd libs! Check docs.

-- | A sample format.
data Format
  = FormatU8      -- ^ Unsigned 8-bit samples.
  | FormatS8      -- ^ Signed 8-bit samples.
  | FormatU16_LSB -- ^ Unsigned 16-bit samples, in little-endian byte order.
  | FormatS16_LSB -- ^ Signed 16-bit samples, in little-endian byte order.
  | FormatU16_MSB -- ^ Unsigned 16-bit samples, in big-endian byte order.
  | FormatS16_MSB -- ^ signed 16-bit samples, in big-endian byte order.
  | FormatU16_Sys -- ^ Unsigned 16-bit samples, in system byte order.
  | FormatS16_Sys -- ^ Signed 16-bit samples, in system byte order.
  deriving (Eq, Ord, Bounded, Read, Show)

formatToWord :: Format -> SDL.Raw.Mixer.Format
formatToWord = \case
  FormatU8      -> SDL.Raw.Mixer.AUDIO_U8
  FormatS8      -> SDL.Raw.Mixer.AUDIO_S8
  FormatU16_LSB -> SDL.Raw.Mixer.AUDIO_U16LSB
  FormatS16_LSB -> SDL.Raw.Mixer.AUDIO_S16LSB
  FormatU16_MSB -> SDL.Raw.Mixer.AUDIO_U16MSB
  FormatS16_MSB -> SDL.Raw.Mixer.AUDIO_S16MSB
  FormatU16_Sys -> SDL.Raw.Mixer.AUDIO_U16SYS
  FormatS16_Sys -> SDL.Raw.Mixer.AUDIO_S16SYS

wordToFormat :: SDL.Raw.Mixer.Format -> Format
wordToFormat = \case
  SDL.Raw.Mixer.AUDIO_U8     -> FormatU8
  SDL.Raw.Mixer.AUDIO_S8     -> FormatS8
  SDL.Raw.Mixer.AUDIO_U16LSB -> FormatU16_LSB
  SDL.Raw.Mixer.AUDIO_S16LSB -> FormatS16_LSB
  SDL.Raw.Mixer.AUDIO_U16MSB -> FormatU16_MSB
  SDL.Raw.Mixer.AUDIO_S16MSB -> FormatS16_MSB
  SDL.Raw.Mixer.AUDIO_U16SYS -> FormatU16_Sys
  SDL.Raw.Mixer.AUDIO_S16SYS -> FormatS16_Sys
  _ -> error "SDL.Mixer.wordToFormat: unknown Format."

-- | An audio configuration. Use this with 'openAudio'.
data Audio = Audio
  { audioFrequency :: Int    -- ^ A sampling frequency.
  , audioFormat    :: Format -- ^ An output sample format.
  , audioOutput    :: Output -- ^ 'Mono' or 'Stereo' output.
  } deriving (Eq, Read, Show)

instance Default Audio where
  def = Audio { audioFrequency = SDL.Raw.Mixer.MIX_DEFAULT_FREQUENCY
              , audioFormat    = FormatS16_Sys
              , audioOutput    = Stereo
              }

-- | The number of sound channels in output.
data Output = Mono | Stereo
  deriving (Eq, Ord, Bounded, Read, Show)

outputToCInt :: Output -> CInt
outputToCInt = \case
  Mono   -> 1
  Stereo -> 2

cIntToOutput :: CInt -> Output
cIntToOutput = \case
  1 -> Mono
  2 -> Stereo
  _ -> error "SDL.Mixer.cIntToOutput: unknown number of channels."

-- | The size of each mixed sample. The smaller this is, the more your hooks
-- will be called. If this is made too small on a slow system, the sounds may
-- skip. If made too large, sound effects could lag.
type ChunkSize = Int

-- | Initializes the @SDL2_mixer@ API. This should be the first function you
-- call after intializing @SDL@ itself with 'SDL.Init.InitAudio'.
openAudio :: (Functor m, MonadIO m) => Audio -> ChunkSize -> m ()
openAudio (Audio {..}) chunkSize =
  throwIfNeg_ "SDL.Mixer.openAudio" "Mix_OpenAudio" $
    SDL.Raw.Mixer.openAudio
      (fromIntegral audioFrequency)
      (formatToWord audioFormat)
      (outputToCInt audioOutput)
      (fromIntegral chunkSize)

-- | Shut down and clean up the @SDL2_mixer@ API. After calling this, all audio
-- stops and no functions except 'openAudio' should be used.
closeAudio :: MonadIO m => m ()
closeAudio = SDL.Raw.Mixer.closeAudio

-- | Get the audio format in use by the opened audio device. This may or may
-- not match the 'Audio' you asked for when calling 'openAudio'.
queryAudio :: (MonadIO m) => m Audio
queryAudio =
  liftIO .
    alloca $ \freq ->
      alloca $ \form ->
        alloca $ \chan -> do
          void . throwIf0 "SDL.Mixer.queryAudio" "Mix_QuerySpec" $
            SDL.Raw.Mixer.querySpec freq form chan
          Audio
            <$> (fromIntegral <$> peek freq)
            <*> (wordToFormat <$> peek form)
            <*> (cIntToOutput <$> peek chan)

-- -- | An audio chunk.
-- newtype Chunk = Chunk (Ptr SDL.Raw.Mixer.Chunk)

-- Chunks
-- TODO: getNumChunkDecoders
-- TODO: getChunkDecoder
-- TODO: loadWAV
-- TODO: loadWAV_RW
-- TODO: quickLoadWAV
-- TODO: quickLoadRaw
-- TODO: volumeChunk
-- TODO: freeChunk

-- Channels
-- TODO: allocateChannels
-- TODO: volume
-- TODO: playChannel
-- TODO: playChannelTimed
-- TODO: fadeInChannel
-- TODO: fadeInChannelTimed
-- TODO: pause
-- TODO: resume
-- TODO: haltChannel
-- TODO: expireChannel
-- TODO: fadeOutChannel
-- TODO: channelFinished
-- TODO: playing
-- TODO: paused
-- TODO: fadingChannel
-- TODO: getChunk

-- Channel groups
-- TODO: reserveChannels
-- TODO: groupChannel
-- TODO: groupChannels
-- TODO: groupCount
-- TODO: groupAvailable
-- TODO: groupOldest
-- TODO: groupNewest
-- TODO: fadeOutGroup
-- TODO: haltGroup

-- Music
-- TODO: getNumMusicDecoders
-- TODO: getMusicDecoder
-- TODO: loadMUS
-- TODO: loadMUS_RW
-- TODO: loadMUSType_RW
-- TODO: freeMusic
-- TODO: playMusic
-- TODO: fadeInMusic
-- TODO: fadeInMusicPos
-- TODO: hookMusic
-- TODO: volumeMusic
-- TODO: pauseMusic
-- TODO: resumeMusic
-- TODO: rewindMusic
-- TODO: setMusicPosition
-- TODO: setMusicCMD
-- TODO: haltMusic
-- TODO: fadeOutMusic
-- TODO: hookMusicFinished
-- TODO: getMusicType
-- TODO: playingMusic
-- TODO: pausedMusic
-- TODO: fadingMusic
-- TODO: getMusicHookData

-- Effects
-- TODO: registerEffect
-- TODO: unregisterEffect
-- TODO: unregisterAllEffects
-- TODO: setPostMix
-- TODO: setPanning
-- TODO: setDistance
-- TODO: setPosition
-- TODO: setReverseStereo

-- SoundFonts
-- TODO: setSynchroValue
-- TODO: getSynchroValue
-- TODO: setSoundFonts
-- TODO: getSoundFonts
-- TODO: eachSoundFont

-- load :: (Functor m, MonadIO m) => FilePath -> m Chunk
-- load filePath =
--   fmap Chunk $
--     throwIfNull "SDL.Mixer.load" "Mix_LoadWAV" $
--       liftIO $ withCString filePath $ \cstr ->
--         Raw.loadWav cstr

-- newtype Channel = Channel CInt

-- data ChannelChoice
--   = AnyChannel
--   | SpecificChannel Channel

-- data Loops
--   = Infinite
--   | Once
--   | Repeat Int

-- play :: (Functor m, MonadIO m) => Chunk -> m Channel
-- play chunk = playChannel AnyChannel chunk Once

-- playChannel :: (Functor m, MonadIO m) => ChannelChoice -> Chunk -> Loops -> m Channel
-- playChannel channel chunk loops =
--   fmap Channel $
--     throwIfNeg "SDL.Mixer.playChannel" "Mix_PlayChannel" $
--       SDL.Raw.Mixer.playChannel channel' chunk' loops'
--   where
--     Chunk chunk' = chunk
--     channel' = case channel of
--                     AnyChannel                  -> -1
--                     SpecificChannel (Channel n) -> n
--     loops' = case loops of
--                   Infinite             -> (-1)
--                   Once                 -> 0
--                   Repeat n | n > 1     -> fromIntegral (n - 1)
--                            | otherwise -> error "Invalid Repeat value"

-- playing :: (Functor m, MonadIO m) => Channel -> m Bool
-- playing channel =
--   fmap (> 0) $
--     SDL.Raw.Mixer.playing channel'
--   where
--     Channel channel' = channel

-- playingCount :: (Functor m, MonadIO m) => m Int
-- playingCount =
--   fmap fromIntegral $
--     SDL.Raw.Mixer.playing (-1)

-- freeChunk :: MonadIO m => Chunk -> m ()
-- freeChunk chunk = SDL.Raw.Mixer.freeChunk chunk'
--   where
--     Chunk chunk' = chunk
