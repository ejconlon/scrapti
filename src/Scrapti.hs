module Scrapti where

import Control.Monad.Trans.Resource (MonadResource, allocate, runResourceT)
import qualified SDL

exe :: IO ()
exe = putStrLn "hello, world"

playSound :: IO ()
playSound = runResourceT $ do
  sdlInit

alloc :: MonadResource m => IO a -> (a -> IO ()) -> m a
alloc allocRes freeRes = fmap snd (allocate allocRes freeRes)

sdlInit :: MonadResource m => m ()
sdlInit = alloc x y where
  x = SDL.initialize [SDL.InitAudio]
  y = const SDL.quit
