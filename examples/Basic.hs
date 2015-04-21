import qualified SDL.Mixer as Mix
import qualified SDL

import Data.Default.Class
import System.Environment
import System.Exit

main :: IO ()
main = do
  -- read arguments
  fileName <- do
    args <- getArgs
    case args of
      (arg:_) -> return arg
      _ -> do
        putStrLn "Usage: cabal run sdl2-mixer-basic <sound filename>"
        exitWith $ ExitFailure 1

  -- initialize libraries
  SDL.initialize [SDL.InitAudio]
  Mix.initialize [Mix.InitMP3]

  -- open device
  Mix.openAudio def 256

  -- open file
  sound <- Mix.load fileName

  -- play file
  Mix.play sound

  -- wait until finished
  whileTrueM $ Mix.playing Mix.AnyChannel

 -- free resources
  Mix.free sound

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
