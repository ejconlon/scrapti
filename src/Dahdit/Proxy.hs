module Dahdit.Proxy
  ( Proxy (..)
  , proxyFor
  , proxyForF
  , proxyForFun
  ) where

import Data.Proxy (Proxy (..))

proxyFor :: a -> Proxy a
proxyFor _ = Proxy

proxyForF :: f a -> Proxy a
proxyForF _ = Proxy

proxyForFun :: (a -> x) -> Proxy a
proxyForFun _ = Proxy
