{-# LANGUAGE RecordWildCards #-}

module SDL.Raw.Mixer.Types
  ( InitFlag
  , Format
  , Tag
  , Channel
  , Fading
  , ChannelFinishedCallback
  , EffectCallback
  , EffectDoneCallback
  , MixCallback
  , MusicFinishedCallback
  , Music
  , MusicType
  , Chunk(..)
  ) where

#include "SDL_mixer.h"

import Data.Word        (Word8, Word16, Word32)
import Foreign.C.Types  (CInt)
import Foreign.Ptr      (Ptr, FunPtr)
import Foreign.Storable (Storable(..))

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
