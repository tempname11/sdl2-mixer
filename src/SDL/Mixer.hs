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

module SDL.Mixer where

import Prelude hiding (foldl)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.|.), (.&.))
import Data.Default.Class (Default(def))
import Data.Foldable (foldl)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import SDL.Exception (throwIfNeg_, throwIf_, throwIf0)

import qualified SDL.Raw
import qualified SDL.Raw.Mixer

-- | Gets the major, minor, patch versions of the linked @SDL2_mixer@ library.
version :: (Integral a, MonadIO m) => m (a, a, a)
version = liftIO $ do
  SDL.Raw.Version major minor patch <- peek =<< SDL.Raw.Mixer.getVersion
  return (fromIntegral major, fromIntegral minor, fromIntegral patch)

-- | Initialize the library by loading support for a certain set of
-- sample/music formats. You may call this function multiple times.
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

data Format
  = FormatU8
  | FormatS8
  | FormatU16_LSB
  | FormatS16_LSB
  | FormatU16_MSB
  | FormatS16_MSB
  | FormatU16
  | FormatS16
  | FormatU16_Sys
  | FormatS16_Sys
  deriving (Eq, Ord, Bounded, Read, Show)

formatToWord :: Format -> SDL.Raw.Mixer.Format
formatToWord = \case
  FormatU8      -> SDL.Raw.Mixer.AUDIO_U8
  FormatS8      -> SDL.Raw.Mixer.AUDIO_S8
  FormatU16_LSB -> SDL.Raw.Mixer.AUDIO_U16LSB
  FormatS16_LSB -> SDL.Raw.Mixer.AUDIO_S16LSB
  FormatU16_MSB -> SDL.Raw.Mixer.AUDIO_U16MSB
  FormatS16_MSB -> SDL.Raw.Mixer.AUDIO_S16MSB
  FormatU16     -> SDL.Raw.Mixer.AUDIO_U16
  FormatS16     -> SDL.Raw.Mixer.AUDIO_S16
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

data AudioSpec = AudioSpec
  { audioFrequency :: Int    -- ^ Sampling frequency.
  , audioFormat    :: Format -- ^ Output sample format.
  , audioOutput    :: Output -- ^ 'Mono' or 'Stereo' output.
  } deriving (Eq, Read, Show)

instance Default AudioSpec where
  def = AudioSpec { audioFrequency = SDL.Raw.Mixer.MIX_DEFAULT_FREQUENCY
                  , audioFormat    = FormatS16_Sys
                  , audioOutput    = Stereo
                  }

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

-- | Initializes the @SDL2_mixer@ API. This must be the first function you call
-- after intializing @SDL@ itself with 'SDL.InitAudio'.
openAudio :: (Functor m, MonadIO m) => AudioSpec -> ChunkSize -> m ()
openAudio (AudioSpec {..}) chunkSize =
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
-- not match the 'AudioSpec' you asked for when calling 'openAudio'.
querySpec :: (MonadIO m) => m AudioSpec
querySpec =
  liftIO .
    alloca $ \freq ->
      alloca $ \form ->
        alloca $ \chan -> do
          void . throwIf0 "SDL.Mixer.querySpec" "Mix_QuerySpec" $
            SDL.Raw.Mixer.querySpec freq form chan
          AudioSpec
            <$> (fromIntegral <$> peek freq)
            <*> (wordToFormat <$> peek form)
            <*> (cIntToOutput <$> peek chan)

newtype Chunk = Chunk (Ptr SDL.Raw.Mixer.Chunk)

-- load :: (Functor m, MonadIO m) => FilePath -> m Chunk
-- load filePath =
--   fmap Chunk $
--     throwIfNull "SDL.Mixer.load" "Mix_LoadWAV" $
--       liftIO $ withCString filePath $ \cstr ->
--         Raw.loadWav cstr

newtype Channel = Channel CInt

data ChannelChoice
  = AnyChannel
  | SpecificChannel Channel

data Loops
  = Infinite
  | Once
  | Repeat Int

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

playing :: (Functor m, MonadIO m) => Channel -> m Bool
playing channel =
  fmap (> 0) $
    SDL.Raw.Mixer.playing channel'
  where
    Channel channel' = channel

playingCount :: (Functor m, MonadIO m) => m Int
playingCount =
  fmap fromIntegral $
    SDL.Raw.Mixer.playing (-1)

freeChunk :: MonadIO m => Chunk -> m ()
freeChunk chunk = SDL.Raw.Mixer.freeChunk chunk'
  where
    Chunk chunk' = chunk

  -- fromRaw r
  --   | r == Raw.MIX_INIT_FLAC = InitFLAC
  --   | r == Raw.MIX_INIT_MOD  = InitMOD
  --   | r == Raw.MIX_INIT_MP3  = InitMP3
  --   | r == Raw.MIX_INIT_OGG  = InitOGG
  --   | otherwise = error "SDL.Mixer.fromRaw InitFlag: not recognized."

  -- fromRaw r
  --   | r == Raw.AUDIO_U8     = FormatU8
  --   | r == Raw.AUDIO_S8     = FormatS8
  --   | r == Raw.AUDIO_U16LSB = FormatU16_LSB
  --   | r == Raw.AUDIO_S16LSB = FormatS16_LSB
  --   | r == Raw.AUDIO_U16MSB = FormatU16_MSB
  --   | r == Raw.AUDIO_S16MSB = FormatS16_MSB
  --   | r == Raw.AUDIO_U16    = FormatU16
  --   | r == Raw.AUDIO_S16    = FormatS16
  --   | r == Raw.AUDIO_U16SYS = FormatU16_Sys
  --   | r == Raw.AUDIO_S16SYS = FormatS16_Sys
  --   | otherwise = error "SDL.Mixer.fromRaw Format: not recognized."
