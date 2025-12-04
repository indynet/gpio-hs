module Data.Schm.Res where

import Data.Schm.Pin
import GHC.Generics

data Res
     = PinStates [Pin]
     | PinState Bool
     | Error String
     | Ack
     deriving ( Generic
              , Show
              , Ord
              , Eq
              )
