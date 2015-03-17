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

  -- open device
  Mixer.openAudio Mixer.defaultConfig

  -- open file
  sound <- Mixer.load file
  
  -- play file
  channel <- Mixer.play sound

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

