{-|

Module      : SDL.Mixer
License     : BSD3
Stability   : experimental

Bindings to the @SDL2_mixer@ library.

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module SDL.Mixer
  (
  -- * Initialization
    initialize
  , InitFlag(..)
  , quit
  , version

  -- * Configuring audio
  , openAudio
  , Audio(..)
  , defaultAudio
  , ChunkSize
  , Format(..)
  , Output(..)
  , queryAudio
  , closeAudio

  -- * Loading audio data
  , Loadable(..)

  -- * Chunks
  , chunkDecoders
  , Chunk(..)

  -- * Channels
  , Channel
  , setChannels
  , getChannels
  , pattern AnyChannel

  -- * Playing Chunks
  , play
  , playForever
  , Times
  , pattern Once
  , pattern Forever
  , playOn
  , Milliseconds
  , Limit
  , pattern NoLimit
  , playLimit
  , playing
  , playingCount
  , fadeIn
  , fadeInOn
  , fadeInLimit
  , pause
  , pauseAll
  , resume
  , resumeAll

  -- * Music
  , musicDecoders
  , Music(..)

  -- * Setting the volume
  , Volume
  , HasVolume(..)
  , setVolumeAll
  , getVolumeAll

  ) where

import Control.Monad          (void, forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits              ((.|.), (.&.))
import Data.ByteString        (ByteString, readFile)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Default.Class     (Default(def))
import Data.Foldable          (foldl)
import Foreign.C.String       (peekCString)
import Foreign.C.Types        (CInt)
import Foreign.Marshal.Alloc  (alloca)
import Foreign.Ptr            (Ptr, castPtr)
import Foreign.Storable       (Storable(..))
import Prelude         hiding (foldl, readFile)
import SDL.Exception          (throwIfNeg_, throwIf_, throwIf0, throwIfNull, throwIfNeg)
import SDL.Raw.Filesystem     (rwFromConstMem)

import qualified SDL.Raw
import qualified SDL.Raw.Mixer

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
  InitFLAC       -> SDL.Raw.Mixer.INIT_FLAC
  InitMOD        -> SDL.Raw.Mixer.INIT_MOD
  InitMODPlug    -> SDL.Raw.Mixer.INIT_MODPLUG
  InitMP3        -> SDL.Raw.Mixer.INIT_MP3
  InitOGG        -> SDL.Raw.Mixer.INIT_OGG
  InitFluidSynth -> SDL.Raw.Mixer.INIT_FLUIDSYNTH

-- | Cleans up any loaded libraries, freeing memory.
quit :: MonadIO m => m ()
quit = SDL.Raw.Mixer.quit -- FIXME: May not free all init'd libs! Check docs.

-- | Gets the major, minor, patch versions of the linked @SDL2_mixer@ library.
version :: (Integral a, MonadIO m) => m (a, a, a)
version = liftIO $ do
  SDL.Raw.Version major minor patch <- peek =<< SDL.Raw.Mixer.getVersion
  return (fromIntegral major, fromIntegral minor, fromIntegral patch)

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

-- | An audio configuration. Use this with 'openAudio'.
data Audio = Audio
  { audioFrequency :: Int    -- ^ A sampling frequency.
  , audioFormat    :: Format -- ^ An output sample format.
  , audioOutput    :: Output -- ^ 'Mono' or 'Stereo' output.
  } deriving (Eq, Read, Show)

instance Default Audio where
  def = Audio { audioFrequency = SDL.Raw.Mixer.DEFAULT_FREQUENCY
              , audioFormat    = wordToFormat SDL.Raw.Mixer.DEFAULT_FORMAT
              , audioOutput    = cIntToOutput SDL.Raw.Mixer.DEFAULT_CHANNELS
              }

-- | A default 'Audio' configuration. Same as 'Data.Default.Class.def'.
defaultAudio :: Audio
defaultAudio = def

-- | The size of each mixed sample. The smaller this is, the more your hooks
-- will be called. If this is made too small on a slow system, the sounds may
-- skip. If made too large, sound effects could lag.
type ChunkSize = Int

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

-- | Shut down and clean up the @SDL2_mixer@ API. After calling this, all audio
-- stops and no functions except 'openAudio' should be used.
closeAudio :: MonadIO m => m ()
closeAudio = SDL.Raw.Mixer.closeAudio

-- | A class of all values that can be loaded from some source. You can load
-- both 'Chunk's and 'Music' this way. Note that you must call 'openAudio'
-- before using these, since they have to know the audio configuration to
-- properly convert the data for playback.
class Loadable a where

  -- | Load the value from a 'ByteString'.
  decode :: MonadIO m => ByteString -> m a

  -- | Same as 'decode', but loads from a file instead.
  load :: MonadIO m => FilePath -> m a
  load = (decode =<<) . liftIO . readFile

  -- | Frees the value's memory. It should no longer be used. Note that you
  -- shouldn't free those values which are currently playing.
  free :: MonadIO m => a -> m ()

-- | A volume, where 0 is silent and 128 loudest. 'Volume's lesser than 0 or
-- greater than 128 function as if they are 0 and 128, respectively.
type Volume = Int

volumeToCInt :: Volume -> CInt
volumeToCInt = fromIntegral . max 0 . min 128

-- | A class of all values that have a 'Volume'.
class HasVolume a where

  -- | Gets the value's currently set 'Volume'.
  getVolume :: MonadIO m => a -> m Volume

  -- | Sets a value's 'Volume'. If the value is a 'Chunk', the volume setting
  -- only takes effect when the 'Chunk' is used on a 'Channel', being mixed
  -- into the output. In case of a 'Channel', the volume setting takes effect
  -- during the final mix, along with the 'Chunk' volume. For instance, setting
  -- the 'Volume' of a certain 'Channel' to 64 will halve the volume of all
  -- 'Chunk's played on that 'Channel'.
  setVolume :: MonadIO m => Volume -> a -> m ()

-- | Sets all 'Channel's to a given 'Volume'.
setVolumeAll :: MonadIO m => Volume -> m ()
setVolumeAll = void . SDL.Raw.Mixer.volume (-1) . volumeToCInt

-- | Gets the /average/ 'Volume' of all 'Channel's.
getVolumeAll :: MonadIO m => m Volume
getVolumeAll = fmap fromIntegral $ SDL.Raw.Mixer.volume (-1) (-1)

-- | Returns the names of all chunk decoders currently available. These depend
-- on the availability of shared libraries for each of the formats. The list
-- may contain any of the following, and possibly others: @WAVE@, @AIFF@,
-- @VOC@, @OFF@, @FLAC@, @MP3@.
chunkDecoders :: MonadIO m => m [String]
chunkDecoders =
  liftIO $ do
    num <- SDL.Raw.Mixer.getNumChunkDecoders
    forM [0 .. num - 1] $ \i ->
      SDL.Raw.Mixer.getChunkDecoder i >>= peekCString

-- | A loaded audio chunk.
newtype Chunk = Chunk (Ptr SDL.Raw.Mixer.Chunk) deriving (Eq, Show)

instance Loadable Chunk where
  decode bytes = liftIO $ do
    unsafeUseAsCStringLen bytes $ \(cstr, len) -> do
      rw <- rwFromConstMem (castPtr cstr) (fromIntegral len)
      fmap Chunk .
        throwIfNull "SDL.Mixer.decode<Chunk>" "IMG_LoadWAV_RW" $
          SDL.Raw.Mixer.loadWAV_RW rw 0

  free (Chunk p) = liftIO $ SDL.Raw.Mixer.freeChunk p

instance HasVolume Chunk where
  getVolume   (Chunk p) = fmap fromIntegral $ SDL.Raw.Mixer.volumeChunk p (-1)
  setVolume v (Chunk p) = void . SDL.Raw.Mixer.volumeChunk p $ volumeToCInt v

-- | A channel for mixing. The first channel is 0, the second 1 and so on. Note
-- that you cannot use these if you haven't created them in advance with
-- 'setChannels'. The starting 'Volume' of each 'Channel' is the maximum: 128.
newtype Channel = Channel CInt deriving (Eq, Ord, Enum, Integral, Real, Num)

instance Show Channel where
  show = \case
    AnyChannel -> "AnyChannel"
    Channel c  -> "Channel " ++ show c

-- | Prepares a given number of 'Channel's for use. There are 8 such 'Channel's
-- already prepared for use after 'openAudio' is called. You may call this
-- multiple times, even with sounds playing. If allocating - lesser number of
-- 'Channel's in a future call, the higher channels will be stopped, their
-- finished hooks called, and then freed. Passing in 0 or less will therefore
-- stop and free all mixing channels (but any 'Music' will still be playing).
setChannels :: MonadIO m => Int -> m ()
setChannels = void . SDL.Raw.Mixer.allocateChannels . fromIntegral . max 0

-- | Gets the number of 'Channel's currently in use.
getChannels :: MonadIO m => m Int
getChannels = fromIntegral <$> SDL.Raw.Mixer.allocateChannels (-1)

-- | Use this value when you wish to perform an operation on /any/ 'Channel'.
-- For more information, see each of the functions accepting a 'Channel'.
pattern AnyChannel = (-1) :: Channel

instance HasVolume Channel where
  getVolume   (Channel c) = fmap fromIntegral $ SDL.Raw.Mixer.volume c (-1)
  setVolume v (Channel c) = void . SDL.Raw.Mixer.volume c $ volumeToCInt v

-- | Play a 'Chunk' once, using the first available 'Channel'.
play :: MonadIO m => Chunk -> m ()
play = void . playOn AnyChannel Once

-- | Same as 'play', but keeps playing the 'Chunk' forever. Same as 'playOn'
-- 'AnyChannel' 'Forever'.
playForever :: MonadIO m => Chunk -> m ()
playForever = void . playOn AnyChannel Forever

-- | How many times should a certain 'Chunk' be played?
newtype Times = Times CInt deriving (Eq, Ord, Enum, Integral, Real, Num)

-- | A shorthand for playing once.
pattern Once = 1 :: Times

-- | A shorthand for looping a 'Chunk' forever.
pattern Forever = 0 :: Times

-- | Same as 'play', but plays the 'Chunk' using a specific 'Channel' a
-- specific number of 'Times'. If 'AnyChannel' is used, then plays the 'Chunk'
-- using the first available 'Channel'. Returns the 'Channel' which was used.
playOn :: MonadIO m => Channel -> Times -> Chunk -> m Channel
playOn = playLimit NoLimit

-- | A time in milliseconds.
type Milliseconds = Int

-- | An upper limit of time, in milliseconds.
type Limit = Milliseconds

-- | A lack of an upper limit.
pattern NoLimit = (-1) :: Limit

-- | Same as 'playOn', but imposes an upper limit in 'Milliseconds' to how long
-- the 'Chunk' can play. The playing may still stop before the limit is
-- reached. This is the most generic play function variant.
playLimit :: MonadIO m => Limit -> Channel -> Times -> Chunk -> m Channel
playLimit l (Channel c) (Times t) (Chunk p) =
  throwIfNeg "SDL.Mixer.playLimit" "Mix_PlayChannelTimed" $
    fromIntegral <$> SDL.Raw.Mixer.playChannelTimed c p (t - 1) (fromIntegral l)

-- | Returns whether the given 'Channel' is playing or not. If 'AnyChannel' is
-- used, this returns whether /any/ channel is currently playing.
playing :: MonadIO m => Channel -> m Bool
playing (Channel c) = (> 0) <$> SDL.Raw.Mixer.playing c

-- | Returns how many 'Channel's are currently playing.
playingCount :: MonadIO m => m Int
playingCount = fromIntegral <$> SDL.Raw.Mixer.playing (-1)

-- | Same as 'play', but fades in the 'Chunk' by making the 'Channel' 'Volume'
-- start at 0 and rise to a full 128 over the course of a given number of
-- 'Milliseconds'. The 'Chunk' may end playing before the fade-in is complete,
-- if it doesn't last as long as the given fade-in time.
fadeIn :: MonadIO m => Milliseconds -> Chunk -> m ()
fadeIn ms  = void . fadeInOn AnyChannel Once ms

-- | Same as 'fadeIn', but allows you to specify the 'Channel' to play on and
-- how many 'Times' to play it, similar to 'playOn'. If 'AnyChannel' is used,
-- will play the 'Chunk' on the first available 'Channel'. Returns the
-- 'Channel' that was used.
fadeInOn :: MonadIO m => Channel -> Times -> Milliseconds -> Chunk -> m Channel
fadeInOn = fadeInLimit NoLimit

-- | Same as 'fadeInOn', but imposes an upper limit in 'Milliseconds' to how
-- long the 'Chunk' can play, similar to 'playLimit'. This is the most generic
-- fade-in function variant.
fadeInLimit
  :: MonadIO m =>
     Limit -> Channel -> Times -> Milliseconds -> Chunk -> m Channel
fadeInLimit l (Channel c) (Times t) ms (Chunk p) =
  throwIfNeg "SDL.Mixer.fadeInLimit" "Mix_FadeInChannelTimed" $
    fromIntegral <$>
      SDL.Raw.Mixer.fadeInChannelTimed
        c p (t - 1) (fromIntegral ms) (fromIntegral l)

-- | Pauses the given 'Channel', if it is actively playing. It may still be
-- 'halt'ed. If 'AnyChannel' is used, will pause the first 'Channel'.
pause :: MonadIO m => Channel -> m ()
pause (Channel c) = SDL.Raw.Mixer.pause $ max 0 c

-- | Pauses all actively playing 'Channel's. They may still be 'halt'ed.
pauseAll :: MonadIO m => m ()
pauseAll = SDL.Raw.Mixer.pause (-1)

-- | Resumes playing a given 'Channel'. If 'AnyChannel' is used, will resume
-- the first 'Channel'.
resume :: MonadIO m => Channel -> m ()
resume (Channel c) = SDL.Raw.Mixer.resume $ max 0 c

-- | Resumes all paused 'Channel's.
resumeAll :: MonadIO m => m ()
resumeAll = SDL.Raw.Mixer.resume (-1)


-- Channels
-- TODO: resume
-- TODO: haltChannel
-- TODO: expireChannel
-- TODO: fadeOutChannel
-- TODO: channelFinished
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

-- | Returns the names of all music decoders currently available. These depend
-- on the availability of shared libraries for each of the formats. The list
-- may contain any of the following, and possibly others: @WAVE@, @MODPLUG@,
-- @MIKMOD@, @TIMIDITY@, @FLUIDSYNTH@, @NATIVEMIDI@, @OGG@, @FLAC@, @MP3@.
musicDecoders :: MonadIO m => m [String]
musicDecoders =
  liftIO $ do
    num <- SDL.Raw.Mixer.getNumMusicDecoders
    forM [0 .. num - 1] $ \i ->
      SDL.Raw.Mixer.getMusicDecoder i >>= peekCString

-- | A loaded music file.
newtype Music = Music (Ptr SDL.Raw.Mixer.Music) deriving (Eq, Show)

instance Loadable Music where
  decode bytes = liftIO $ do
    unsafeUseAsCStringLen bytes $ \(cstr, len) -> do
      rw <- rwFromConstMem (castPtr cstr) (fromIntegral len)
      fmap Music .
        throwIfNull "SDL.Mixer.decode<Music>" "IMG_LoadMUS_RW" $
          SDL.Raw.Mixer.loadMUS_RW rw 0

  free (Music p) = liftIO $ SDL.Raw.Mixer.freeMusic p

-- Music
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
