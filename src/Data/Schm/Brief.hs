module Data.Schm.Brief where

import GHC.Generics
import Data.Word

data Brief = Brief
        { pins :: Word
        }
        deriving ( Generic
                 , Show
                 , Ord
                 , Eq
                 )
