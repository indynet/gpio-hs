module Data.Schm.Pin where

import GHC.Generics (Generic)
import Data.Word

import qualified Data.Map as Map
import Data.Map (Map)

import Control.Lens

data Pin = Pin
        { label :: Word8
        , value :: Bool
        }
        deriving ( Generic
                 , Show
                 , Ord
                 , Eq
                 )

_Pin :: Iso' Pin (Word8, Bool)
_Pin = iso (\(Pin l b) -> (l, b)) (uncurry Pin)

_Pins :: Iso' [Pin] (Map Word8 Bool)
_Pins = iso toMap ofMap
        where
                ofMap = map (_Pin #) . Map.toList
                toMap pins = Map.fromList $ map (from _Pin #) pins
