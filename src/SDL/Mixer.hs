{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module SDL.Mixer (
  initialize,
  quit,
  openAudio,
  querySpec,
  load,
  play,
  playChannel,
  playing,
  playingCount,
  freeChunk,
  closeAudio,

  defaultSpec,
  AudioSpec(..),

  Output(..),
  Format(..),
  InitFlag(..),
  ChannelChoice(..),
  Loops(..),
  Chunk,
  Channel,
) where

import Prelude hiding (foldl)
import Control.Applicative
import Control.Monad.IO.Class
import Data.Bits
import Data.Foldable
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import SDL.Exception

import qualified SDL.Raw.Mixer as Raw

foldFlags :: (Bits b, Foldable f, Num b) => (flag -> b) -> f flag -> b
foldFlags f = foldl (\a b -> a .|. f b) 0

class RawConversion a where
  type R a :: *
  toRaw :: a -> R a
  fromRaw :: R a -> a

data InitFlag
  = InitFLAC
  | InitMOD
  | InitMP3
  | InitOGG
  deriving (Bounded, Eq, Read, Show)

instance RawConversion InitFlag where
  type R InitFlag = Raw.InitFlag
  toRaw InitFLAC = Raw.MIX_INIT_FLAC
  toRaw InitMOD = Raw.MIX_INIT_MOD
  toRaw InitMP3 = Raw.MIX_INIT_MP3
  toRaw InitOGG = Raw.MIX_INIT_OGG
  fromRaw r = case r of
    r' | r' == Raw.MIX_INIT_FLAC -> InitFLAC 
       | r' == Raw.MIX_INIT_MOD  -> InitMOD 
       | r' == Raw.MIX_INIT_MP3  -> InitMP3 
       | r' == Raw.MIX_INIT_OGG  -> InitOGG 
       | otherwise               -> error "Raw InitFlag not recognized"

initialize :: (Foldable f, Functor m, MonadIO m) => f InitFlag -> m ()
initialize flags =
  throwIf_ ((/= rawFlags) . (.&. rawFlags)) "SDL.Mixer.initialize" "Mix_Init" $
    Raw.init rawFlags
  where
    rawFlags = foldFlags toRaw flags

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
  deriving (Bounded, Eq, Read, Show)

instance RawConversion Format where
  type R Format = Raw.Format
  toRaw FormatU8      = Raw.AUDIO_U8
  toRaw FormatS8      = Raw.AUDIO_S8
  toRaw FormatU16_LSB = Raw.AUDIO_U16LSB
  toRaw FormatS16_LSB = Raw.AUDIO_S16LSB
  toRaw FormatU16_MSB = Raw.AUDIO_U16MSB
  toRaw FormatS16_MSB = Raw.AUDIO_S16MSB
  toRaw FormatU16     = Raw.AUDIO_U16
  toRaw FormatS16     = Raw.AUDIO_S16
  toRaw FormatU16_Sys = Raw.AUDIO_U16SYS
  toRaw FormatS16_Sys = Raw.AUDIO_S16SYS
  fromRaw r = case r of
    r' | r' == Raw.AUDIO_U8     -> FormatU8 
       | r' == Raw.AUDIO_S8     -> FormatS8 
       | r' == Raw.AUDIO_U16LSB -> FormatU16_LSB 
       | r' == Raw.AUDIO_S16LSB -> FormatS16_LSB 
       | r' == Raw.AUDIO_U16MSB -> FormatU16_MSB 
       | r' == Raw.AUDIO_S16MSB -> FormatS16_MSB 
       | r' == Raw.AUDIO_U16    -> FormatU16 
       | r' == Raw.AUDIO_S16    -> FormatS16 
       | r' == Raw.AUDIO_U16SYS -> FormatU16_Sys 
       | r' == Raw.AUDIO_S16SYS -> FormatS16_Sys 
       | otherwise              -> error "Raw Format not recognized"

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
  { audioFrequency :: Int
  , audioFormat    :: Format
  , audioOutput    :: Output
  } deriving (Eq, Read, Show)

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

load :: (Functor m, MonadIO m) => FilePath -> m Chunk
load filePath =
  fmap Chunk $
    throwIfNull "SDL.Mixer.load" "Mix_LoadWAV" $
      liftIO $ withCString filePath $ \cstr ->
        Raw.loadWav cstr

newtype Channel = Channel CInt

data ChannelChoice
  = AnyChannel
  | SpecificChannel Channel

data Loops
  = Infinite
  | Once
  | Repeat Int

play :: (Functor m, MonadIO m) => Chunk -> m Channel
play chunk = playChannel AnyChannel chunk Once

playChannel :: (Functor m, MonadIO m) => ChannelChoice -> Chunk -> Loops -> m Channel
playChannel channel chunk loops =
  fmap Channel $
    throwIfNeg "SDL.Mixer.playChannel" "Mix_PlayChannel" $
      Raw.playChannel channel' chunk' loops'
  where
    Chunk chunk' = chunk
    channel' = case channel of
                    AnyChannel                  -> -1
                    SpecificChannel (Channel n) -> n
    loops' = case loops of
                  Infinite             -> (-1)
                  Once                 -> 0
                  Repeat n | n > 1     -> fromIntegral (n - 1)
                           | otherwise -> error "Invalid Repeat value"

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
