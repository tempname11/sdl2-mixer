module Main where

import Control.Monad      (when)
import Data.Default.Class (def)
import System.Environment (getArgs)
import System.Exit        (exitFailure)

import qualified SDL
import qualified SDL.Mixer as Mix

main :: IO ()
main = do
  SDL.initialize [SDL.InitAudio]
  Mix.openAudio def 256

  putStr "Available chunk decoders: "
  print =<< Mix.chunkDecoders

  args <- getArgs
  case args of
    [] -> putStrLn "Usage: cabal run sdl2-mixer-jumbled FILE..." >> exitFailure
    xs -> runExample xs

  Mix.closeAudio
  SDL.quit

-- | Play each of the sounds at the same time!
runExample :: [FilePath] -> IO ()
runExample paths = do
  Mix.setChannels $ length paths
  chunks <- mapM Mix.load paths
  mapM_ Mix.play chunks
  delayWhile $ Mix.playing Mix.AnyChannel
  Mix.setChannels 0
  mapM_ Mix.free chunks

delayWhile :: IO Bool -> IO ()
delayWhile check = loop'
  where
    loop' = do
      still <- check
      when still $ SDL.delay 100 >> delayWhile check
