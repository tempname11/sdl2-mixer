{-# LANGUAGE OverloadedStrings #-}
module SDL.Mixer (
  initialize,
  quit,
  openAudio,
  load,
  playChannel,
  playing,
  playingCount,
  freeChunk,
  closeAudio,

  Format(..),
  InitFlag(..),
  ChannelChoice(..),
  Loops(..),
  Chunk,
  Channel,
) where

import Prelude hiding (foldl)
import Control.Monad.IO.Class
import Data.Bits
import Data.Foldable
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import SDL.Exception

import qualified SDL.Raw.Mixer as Raw
import qualified SDL.Raw.Mixer.Enum as Raw
import qualified SDL.Raw.Mixer.Types as Raw

foldFlags :: (Bits b, Foldable f, Num b) => (flag -> b) -> f flag -> b
foldFlags f = foldl (\a b -> a .|. f b) 0

data InitFlag
  = InitFLAC
  | InitMOD
  | InitMP3
  | InitOGG
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

initFlagConvert :: InitFlag -> Raw.InitFlag
initFlagConvert = go where
  go InitFLAC = Raw.MIX_INIT_FLAC
  go InitMOD = Raw.MIX_INIT_MOD
  go InitMP3 = Raw.MIX_INIT_MP3
  go InitOGG = Raw.MIX_INIT_OGG

initialize :: (Foldable f, Functor m, MonadIO m) => f InitFlag -> m ()
initialize flags =
  throwIf_ (/= rawFlags) "SDL.Mixer.initialize" "Mix_Init" $
    Raw.init rawFlags
  where
    rawFlags = foldFlags initFlagConvert flags

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
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

formatConvert :: Format -> Raw.Format
formatConvert = go where
  go FormatU8 = Raw.AUDIO_U8
  go FormatS8 = Raw.AUDIO_S8
  go FormatU16_LSB = Raw.AUDIO_U16LSB
  go FormatS16_LSB = Raw.AUDIO_S16LSB
  go FormatU16_MSB = Raw.AUDIO_U16MSB
  go FormatS16_MSB = Raw.AUDIO_S16MSB
  go FormatU16 = Raw.AUDIO_U16
  go FormatS16 = Raw.AUDIO_S16
  go FormatU16_Sys = Raw.AUDIO_U16SYS
  go FormatS16_Sys = Raw.AUDIO_S16SYS

openAudio :: (Functor m, MonadIO m) => Int -> Format -> Int -> Int -> m ()
openAudio frequency format channels chunksize =
  throwIfNeg_ "SDL.Mixer.openAudio" "Mix_OpenAudio" $
    Raw.openAudio frequency' format' channels' chunksize'
    where
      frequency' = fromIntegral frequency
      format' = formatConvert format
      channels' = fromIntegral channels
      chunksize' = fromIntegral chunksize

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
  | Repeat Int

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
                  Repeat n | n >= 0    -> fromIntegral n
                           | otherwise -> undefined

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
