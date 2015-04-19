import qualified SDL.Mixer as Mix
import qualified SDL

import System.Environment
import System.Exit

main :: IO ()
main = do
  -- read arguments
  fileName <- do
    args <- getArgs
    case args of
      [arg] -> return arg
      _ -> do
        putStrLn "Usage: <cmd> <sound filename>"
        exitWith $ ExitFailure 1

  -- initialize libraries
  SDL.initialize [SDL.InitAudio]
  Mix.initialize [Mix.InitMP3]

  -- open device
  Mix.openAudio Mix.defaultSpec 256

  -- open file
  sound <- Mix.load fileName

  -- play file
  channel <- Mix.play sound

  -- wait until finished
  whileTrueM $ Mix.playing channel

 -- free resources
  Mix.freeChunk sound

  -- close device
  Mix.closeAudio

  -- quit
  Mix.quit
  SDL.quit

whileTrueM :: Monad m => m Bool -> m ()
whileTrueM cond = do
  loop <- cond
  if loop then whileTrueM cond
          else return ()
