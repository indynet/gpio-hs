module Control.Effect.Pin
        ( flipPin
        , runPin
        , states
        , labels
        , pins
        , Pin
        , pin
        , lo
        , hi
        ) where

import Effectful.Dispatch.Static
import Effectful

import qualified Data.Schm.Pin as Schm
import Data.Schm.Pin (_Pins)

import qualified Data.Map as Map
import Data.Map (Map)
import Control.Lens
import Data.Word

import Data.Bifunctor

data Fmt = Fmt { _fmt :: Map Word8 Bool }

makeLenses ''Fmt

data Pin :: Effect

type    instance DispatchOf Pin = Static NoSideEffects
newtype instance StaticRep  Pin = Pin Fmt

pins :: Pin :> es => Word8 -> (Bool -> Bool) -> Eff es ()
pins l b = stateStaticRep $ \(Pin r) ->
        let fmt' =  r & fmt %~ (ix l %~ b)
        in ((), Pin fmt')

pin :: Pin :> es => Word8 -> Bool -> Eff es ()
pin = (. const) . pins

lo :: Pin :> es => Word8 -> Eff es ()
lo = (`pin` False)

hi :: Pin :> es => Word8 -> Eff es ()
hi = (`pin` True)

labels :: Pin :> es => Eff es [Word8]
labels = getStaticRep <&> \(Pin r) -> Map.keys (r^.fmt)

states :: Pin :> es => Eff es [Bool]
states = getStaticRep <&> \(Pin r) -> Map.elems (r^.fmt)

flipPin :: Pin :> es => Word8 -> Eff es ()
flipPin l = stateStaticRep $ \(Pin r) ->
        let fmt' = r & fmt %~ (ix l %~ not)
        in ((), Pin fmt')

runPin :: [Schm.Pin] -> Eff (Pin : es) a -> Eff es (a, [Schm.Pin])
runPin pins m = second f <$> runStaticRep (Pin fmt) m
        where
                fmt             = Fmt (from _Pins # pins)
                f (Pin (Fmt r)) = _Pins # r
