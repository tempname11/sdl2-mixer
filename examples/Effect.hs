module Main where

import Data.Int           (Int16)
import Data.Word          (Word8)
import Control.Monad      (when, forM_)
import System.Environment (getArgs)
import System.Exit        (exitFailure)

import qualified SDL
import qualified SDL.Mixer as Mix
import qualified Data.Vector.Storable.Mutable as MV

main :: IO ()
main = do
  SDL.initialize [SDL.InitAudio]

  Mix.withAudio Mix.defaultAudio 256 $ do
    putStr "Available music decoders: "
    print =<< Mix.musicDecoders

    args <- getArgs
    case args of
      [] -> putStrLn "Usage: cabal run sdl2-mixer-effect FILE" >> exitFailure
      xs -> runExample $ head xs

  SDL.quit

-- An effect that does something silly: lowers the volume 2 times.
halveVolume :: MV.IOVector Word8 -> IO ()
halveVolume bytes = do
  let shorts = MV.unsafeCast bytes :: MV.IOVector Int16
  forM_ [0 .. MV.length shorts - 1] $ \i -> do
    s <- MV.read shorts i
    MV.write shorts i $ div s 2

-- Apply an Effect on the Music being played.
runExample :: FilePath -> IO ()
runExample path = do

  -- Add effects, get back effect removal actions.
  -- The volume should be EIGHT times as low after this.
  popEffects <-
    mapM (Mix.effect Mix.PostProcessing (\_ -> return ()) . const)
         [halveVolume, halveVolume, halveVolume]

  music <- Mix.load path
  Mix.playMusic Mix.Once music

  delayWhile Mix.playingMusic

  sequence_ popEffects -- The effects are no longer applied after this.
  Mix.free music

delayWhile :: IO Bool -> IO ()
delayWhile check = loop'
  where
    loop' = do
      still <- check
      when still $ SDL.delay 300 >> delayWhile check
