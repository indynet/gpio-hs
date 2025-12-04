module Data.Schm.Prompt where

import GHC.Generics
import Data.Word

data Prompt = Prompt
        { name :: String
        , pins :: Word
        }
        deriving ( Generic
                 , Show
                 , Ord
                 , Eq
                 )
