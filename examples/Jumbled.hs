module Main where

import Control.Monad      (when)
import Data.Default.Class (def)
import System.Environment (getArgs)
import System.Exit        (exitFailure)

import qualified SDL
import qualified SDL.Mixer

main :: IO ()
main = do
  SDL.initialize [SDL.InitAudio]
  SDL.Mixer.openAudio def 256

  putStr "Available chunk decoders: "
  print =<< SDL.Mixer.chunkDecoders

  args <- getArgs
  case args of
    [] -> putStrLn "usage: cabal run sdl2-mixer-jumbled FILE..." >> exitFailure
    xs -> runExample xs

  SDL.Mixer.closeAudio
  SDL.quit

-- | Play each of the sounds at the same time!
runExample :: [FilePath] -> IO ()
runExample paths = do
  SDL.Mixer.setChannels $ length paths
  chunks <- mapM SDL.Mixer.load paths
  mapM_ (SDL.Mixer.play SDL.Mixer.AnyChannel SDL.Mixer.Once) chunks
  delayWhile $ SDL.Mixer.playing SDL.Mixer.AnyChannel
  SDL.Mixer.setChannels 0
  mapM_ SDL.Mixer.free chunks

delayWhile :: IO Bool -> IO ()
delayWhile check = loop'
  where
    loop' = do
      still <- check
      when still $ SDL.delay 100 >> delayWhile check
