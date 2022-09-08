{-# LANGUAGE UndecidableInstances #-}

module Scrapti.Patches.Meta where

import Data.Text (Text)
import Data.Sequence (Seq)
import GHC.Generics (Generic (..))
import Data.Aeson (genericToJSON, ToJSON (..), FromJSON (..), genericParseJSON, GToJSON', Value, Zero, GFromJSON)
import Data.Aeson.Casing (aesonPrefix, snakeCase)

newtype SnakeRecord a = SnakeRecord { unSnakeRecord :: a }

instance (Generic a, GToJSON' Value Zero (Rep a)) => ToJSON (SnakeRecord a) where
  toJSON = genericToJSON (aesonPrefix snakeCase) . unSnakeRecord

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (SnakeRecord a) where
  parseJSON = fmap SnakeRecord . genericParseJSON (aesonPrefix snakeCase)

data Patch = Patch
  { pxPatchSlug :: !Text
  , pxDesc :: !Text
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (SnakeRecord Patch)

data Version = Version
  { vxVersion :: !Text
  , vxChanges :: !(Seq Text)
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (SnakeRecord Version)

data Meta = Meta
  { mxPackName :: !Text
  , mxPackSlug :: !Text
  , mxPatches :: !(Seq Patch)
  , mxChangelog :: !(Seq Version)
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (SnakeRecord Meta)
