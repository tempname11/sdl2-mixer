import qualified SDL.Mixer as Mixer
import qualified SDL

import System.Environment

main :: IO ()
main = do
  -- read arguments
  args <- getArgs

  let file = case args of
             [arg] -> arg
             _ -> "sound.wav"

  -- initialize libraries
  SDL.initialize [SDL.InitAudio]
  Mixer.initialize [Mixer.InitMP3]

  let rate = 22050
      format = Mixer.FormatS16_Sys
      channels = 2
      bufsize = 4096

  -- open device
  Mixer.openAudio rate format channels bufsize

  -- open file
  sound <- Mixer.load file
  
  -- play file
  channel <- Mixer.playChannel Mixer.AnyChannel sound (Mixer.Repeat 0)

  -- wait until finished
  whileTrueM $ Mixer.playing channel

 -- free resources
  Mixer.freeChunk sound

  -- close device
  Mixer.closeAudio

  -- quit
  Mixer.quit
  SDL.quit

whileTrueM :: Monad m => m Bool -> m ()
whileTrueM cond = do
  loop <- cond
  if loop then whileTrueM cond
          else return ()

