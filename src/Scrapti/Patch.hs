module Scrapti.Patch where

-- data PtiInst = PtiInst !Text !Text !Pti
--   deriving (Eq, Show)

-- instToHeader :: Maybe LoopMarkPoints -> Int -> InstParams -> Either String Header
-- instToHeader mayPoints noteOffset params = do
--   let !hd = def @Header
--   error "TODO"

-- instToPti :: Maybe LoopMarkNames -> Text -> Text -> Int -> InstParams -> Sample -> IO Pti
-- instToPti mayNames instName noteName noteCenterVal params sample = do
--   (arr, mayPoints) <- readPtiWav mayNames (samplePath sample)
--   unless (isNothing mayNames || isJust mayPoints) (fail "did not find expected loop points")
--   let !header = undefined
--   pure $! mkPti header arr

-- instToPtis :: InstSpec FilePath -> Seq Sample -> IO (Seq PtiInst)
-- instToPtis = error "TODO"
