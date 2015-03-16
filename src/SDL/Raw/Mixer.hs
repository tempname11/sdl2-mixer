module SDL.Raw.Mixer (
  init,
  openAudio,
  loadWav,
  playChannel,
  playing,
  freeChunk,
  closeAudio,

  module SDL.Raw.Mixer.Enum,
  module SDL.Raw.Mixer.Types
) where

import Prelude hiding (init)
import Control.Monad.IO.Class
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import SDL.Raw.Mixer.Enum
import SDL.Raw.Mixer.Types
import SDL.Raw.Types (RWops(..))
import SDL.Raw.Filesystem (rwFromFile)

foreign import ccall "SDL_mixer.h Mix_Init" init' :: InitFlag -> IO CInt
foreign import ccall "SDL_mixer.h Mix_OpenAudio" openAudio' :: CInt -> Format -> CInt -> CInt -> IO CInt
foreign import ccall "SDL_mixer.h Mix_LoadWAV_RW" loadWavRw' :: Ptr RWops -> CInt -> IO (Ptr Chunk)
foreign import ccall "SDL_mixer.h Mix_PlayChannelTimed" playChannelTimed' :: CInt -> Ptr Chunk -> CInt -> CInt -> IO CInt
foreign import ccall "SDL_mixer.h Mix_Playing" playing' :: CInt -> IO CInt
foreign import ccall "SDL_mixer.h Mix_FreeChunk" freeChunk' :: Ptr Chunk -> IO ()
foreign import ccall "SDL_mixer.h Mix_CloseAudio" closeAudio' :: IO ()

init :: MonadIO m => InitFlag -> m CInt
init v1 = liftIO $ init' v1
{-# INLINE init #-}

openAudio :: MonadIO m => CInt -> Format -> CInt -> CInt -> m CInt
openAudio v1 v2 v3 v4 = liftIO $ openAudio' v1 v2 v3 v4
{-# INLINE openAudio #-}

loadWavRw :: MonadIO m => Ptr RWops -> CInt -> m (Ptr Chunk)
loadWavRw v1 v2 = liftIO $ loadWavRw' v1 v2
{-# INLINE loadWavRw #-}

loadWav :: MonadIO m => CString -> m (Ptr Chunk)
loadWav v1 = liftIO $ withCString "rb" $ \rb -> do
  file <- rwFromFile v1 rb
  loadWavRw' (file) 1

{-# INLINE loadWav #-}

playChannel :: MonadIO m => CInt -> Ptr Chunk -> CInt -> m CInt
playChannel v1 v2 v3 = liftIO $ playChannelTimed' v1 v2 v3 (-1)
{-# INLINE playChannel #-}

playChannelTimed :: MonadIO m => CInt -> Ptr Chunk -> CInt -> CInt -> m CInt
playChannelTimed v1 v2 v3 v4 = liftIO $ playChannelTimed' v1 v2 v3 v4
{-# INLINE playChannelTimed #-}

playing :: MonadIO m => CInt -> m CInt
playing v1 = liftIO $ playing' v1
{-# INLINE playing #-}

freeChunk :: MonadIO m => Ptr Chunk -> m ()
freeChunk v1 = liftIO $ freeChunk' v1
{-# INLINE freeChunk #-}

closeAudio :: MonadIO m => m ()
closeAudio = liftIO closeAudio'
{-# INLINE closeAudio #-}
