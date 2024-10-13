module Brave.Auth (SubscriptionToken (..)) where

import GHC.Generics (Generic)

import Data.ByteString (ByteString)

-- This is a newtype around text
newtype SubscriptionToken = SubscriptionToken ByteString deriving (Show, Generic)
