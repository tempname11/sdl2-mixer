import qualified SDL.Raw.Mixer as Mixer
import qualified SDL

import Control.Applicative
import Foreign.C.String
import Foreign.Ptr
import System.Environment

main :: IO ()
main = do
  args <- getArgs

  let file = case args of
             [arg] -> arg
             _ -> "sound.wav"

  -- initialize libraries
  SDL.initialize [SDL.InitAudio]
  _ <- Mixer.init Mixer.MIX_INIT_MP3

  let rate = 22050
      format = Mixer.AUDIO_S16SYS
      channels = 2
      bufsize = 4096

  -- open device
  result <- Mixer.openAudio rate format channels bufsize
  assert $ result == 0

  -- open file
  sound <- withCString file $ \cstr -> Mixer.loadWav cstr
  assert $ sound /= nullPtr
  
  -- play file
  channel <- Mixer.playChannel (-1) sound 0
  assert $ channel /= -1

  -- wait until finished
  whileTrueM $ (/= 0) <$> Mixer.playing channel

 -- free resources
  Mixer.freeChunk sound

  -- close device
  Mixer.closeAudio

  SDL.quit

assert :: Bool -> IO ()
assert x = if x
           then return ()
           else undefined

whileTrueM :: Monad m => m Bool -> m ()
whileTrueM cond = do
  loop <- cond
  if loop then whileTrueM cond
          else return ()

