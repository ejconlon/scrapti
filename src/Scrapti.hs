module Scrapti where

import Control.Monad.Trans.Resource (MonadResource, allocate, runResourceT)
import qualified Data.ByteString.Lazy as BSL
import Scrapti.Binary (decodeFail)
import Scrapti.Sample (Sampled)
import Scrapti.Wav (Wav, decodeAnyWav)
import qualified SDL

exe :: IO ()
exe = do
  bs <- BSL.readFile "testdata/drums.wav"
  swav <- decodeFail bs decodeAnyWav
  playSound swav

playSound :: Sampled Wav -> IO ()
playSound _swav = runResourceT $ do
  sdlInit

alloc :: MonadResource m => IO a -> (a -> IO ()) -> m a
alloc allocRes freeRes = fmap snd (allocate allocRes freeRes)

sdlInit :: MonadResource m => m ()
sdlInit = alloc x y where
  x = SDL.initialize [SDL.InitAudio]
  y = const SDL.quit
