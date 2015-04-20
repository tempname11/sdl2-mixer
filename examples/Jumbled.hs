module Main where

import Data.Default.Class (def)

import qualified SDL
import qualified SDL.Mixer

main :: IO ()
main = do
  SDL.initialize [SDL.InitAudio]
  SDL.Mixer.initialize [SDL.Mixer.InitMP3]
  SDL.Mixer.openAudio def 256

  putStr "Available music decoders: "
  print =<< SDL.Mixer.musicDecoders

  SDL.Mixer.closeAudio
  SDL.Mixer.quit
  SDL.quit
