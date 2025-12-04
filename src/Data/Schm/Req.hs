module Data.Schm.Req where

import Data.Schm.Pin

import GHC.Generics
import Data.Word

data Req
     = ModifyPins [Pin]
     | PinState Word8
     | PinStates
     deriving ( Generic
              , Show
              , Ord
              , Eq
              )
