module Scrapti.Aiff where

import Scrapti.Riff (Chunk)

-- AIFF-C file parsing according to
-- http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/AIFF/AIFF.html
-- We only bother to support "sowt" compression (little endian samples)

-- AIFF shares a similar 4-byte label + size + payload structure with RIFF
-- We could use a lot of the same structures to read the file... If they were
-- big-endian.

data AiffFormatBody = AiffFormatBody
  deriving stock (Eq, Show)

type AiffFormatChunk = Chunk AiffFormatBody

-- data Aiff a = Aiff
--   { aiffFormat :: !AiffFormatChunk
--   , aiffBody :: !(AiffBody a)
--   } deriving stock (Eq, Show)
