{-|

Module      : SDL.Mixer
License     : BSD3
Stability   : experimental

Bindings to the @SDL2_mixer@ library.

-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module SDL.Mixer where

import Prelude hiding (foldl)
import Control.Monad.IO.Class
import Data.Bits
import Data.Default.Class (Default(def))
import Data.Foldable
-- import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import SDL.Exception

import qualified SDL.Raw
import qualified SDL.Raw.Mixer
import qualified SDL.Raw.Mixer as Raw

-- | Gets the major, minor, patch versions of the linked @SDL2_mixer@ library.
version :: (Integral a, MonadIO m) => m (a, a, a)
version = liftIO $ do
  SDL.Raw.Version major minor patch <- peek =<< SDL.Raw.Mixer.getVersion
  return (fromIntegral major, fromIntegral minor, fromIntegral patch)

initialize :: (Foldable f, Functor m, MonadIO m) => f InitFlag -> m ()
initialize flags = do
  let raw = foldl (\a b -> a .|. toRaw b) 0 flags
  throwIf_ ((/= raw) . (.&. raw)) "SDL.Mixer.initialize" "Mix_Init" $
    Raw.init raw

class RawConversion a where
  type R a :: *
  toRaw :: a -> R a
  fromRaw :: R a -> a

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

quit :: MonadIO m => m ()
quit = Raw.quit

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

instance RawConversion Format where
  type R Format = Raw.Format

  toRaw = \case
    FormatU8      -> Raw.AUDIO_U8
    FormatS8      -> Raw.AUDIO_S8
    FormatU16_LSB -> Raw.AUDIO_U16LSB
    FormatS16_LSB -> Raw.AUDIO_S16LSB
    FormatU16_MSB -> Raw.AUDIO_U16MSB
    FormatS16_MSB -> Raw.AUDIO_S16MSB
    FormatU16     -> Raw.AUDIO_U16
    FormatS16     -> Raw.AUDIO_S16
    FormatU16_Sys -> Raw.AUDIO_U16SYS
    FormatS16_Sys -> Raw.AUDIO_S16SYS

  fromRaw r
    | r == Raw.AUDIO_U8     = FormatU8
    | r == Raw.AUDIO_S8     = FormatS8
    | r == Raw.AUDIO_U16LSB = FormatU16_LSB
    | r == Raw.AUDIO_S16LSB = FormatS16_LSB
    | r == Raw.AUDIO_U16MSB = FormatU16_MSB
    | r == Raw.AUDIO_S16MSB = FormatS16_MSB
    | r == Raw.AUDIO_U16    = FormatU16
    | r == Raw.AUDIO_S16    = FormatS16
    | r == Raw.AUDIO_U16SYS = FormatU16_Sys
    | r == Raw.AUDIO_S16SYS = FormatS16_Sys
    | otherwise = error "SDL.Mixer.fromRaw Format: not recognized."

defaultSpec :: AudioSpec
defaultSpec = AudioSpec
  { audioFrequency = 44100
  , audioFormat    = FormatS16_Sys
  , audioOutput    = Stereo
  }

data Output
  = Stereo
  | Mono
  deriving (Bounded, Eq, Read, Show)

instance RawConversion Output where
  type R Output = CInt
  toRaw Stereo = 2
  toRaw Mono   = 1
  fromRaw 2 = Stereo
  fromRaw 1 = Mono
  fromRaw _ = error "Raw Output not recognized"

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

openAudio :: (Functor m, MonadIO m) => AudioSpec -> Int -> m ()
openAudio config chunkSize_ =
  throwIfNeg_ "SDL.Mixer.openAudio" "Mix_OpenAudio" $
    Raw.openAudio frequency format output chunkSize
    where
      frequency = fromIntegral (audioFrequency config)
      format    = toRaw        (audioFormat config)
      output    = toRaw        (audioOutput config)
      chunkSize = fromIntegral chunkSize_

querySpec :: (MonadIO m) => m AudioSpec
querySpec = liftIO $
  alloca $ \pa ->
    alloca $ \pb ->
      alloca $ \pc -> do
        _ <- throwIf0 "SDL.Mixer.querySpec" "Mix_QuerySpec" $ Raw.querySpec pa pb pc
        frequency <- fromIntegral <$> peek pa
        format    <- fromRaw      <$> peek pb
        output    <- fromRaw      <$> peek pc
        return $ AudioSpec frequency format output

newtype Chunk = Chunk (Ptr Raw.Chunk)

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
--       Raw.playChannel channel' chunk' loops'
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
    Raw.playing channel'
  where
    Channel channel' = channel

playingCount :: (Functor m, MonadIO m) => m Int
playingCount =
  fmap fromIntegral $
    Raw.playing (-1)

freeChunk :: MonadIO m => Chunk -> m ()
freeChunk chunk = Raw.freeChunk chunk'
  where
    Chunk chunk' = chunk

closeAudio :: MonadIO m => m ()
closeAudio = Raw.closeAudio
