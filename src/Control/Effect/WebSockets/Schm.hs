module Control.Effect.WebSockets.Schm
        ( pinStates
        , pinState
        , Fallible
        , runSchm
        , runApp
        , pinsTo
        , pinsHi
        , pinsLo
        , gpios
        , Host
        , Port
        , gpio
        , wait
        , Schm
        ) where

import Control.Concurrent
import Control.Effect.Pin

import qualified Data.ByteString as B

import qualified Data.Schm.Res    as Schm.Res
import qualified Data.Schm.Req    as Schm.Req
import qualified Data.Schm.Prompt as Schm
import qualified Data.Schm.Brief  as Schm
import qualified Data.Schm.Res    as Schm
import qualified Data.Schm.Req    as Schm
import qualified Data.Schm.Pin    as Schm

import Control.Monad
import Data.Aeson
import Data.Schm

import Effectful.Dispatch.Static
import Effectful.Reader.Static
import Effectful.Error.Static
import Network.WebSockets
import Effectful
import Data.Word

import Control.Lens
import Data.Functor

data Schema = Schema { getConnection :: Connection }

data Schm :: Effect

data SchmError
     = PutJSON String
     | ObjDiff String
     | Failure String
     deriving Show

type    instance DispatchOf Schm = Static WithSideEffects
newtype instance StaticRep  Schm = Schm Schema

type Fallible es = ( Error SchmError :> es
                   , Schm            :> es
                   )

withConnection :: Schm :> es => (Connection -> IO a) -> Eff es a
withConnection f = do
        Schm schm <- getStaticRep
        unsafeEff_ $ (f . getConnection) schm

closing :: Schm :> es => Eff es ()
closing = withConnection $ \c -> sendClose c (B.pack [])

getJSON :: (FromJSON a, Fallible es) => Eff es a
getJSON = withConnection (receiveData @B.ByteString) >>= f
        where f m = case eitherDecodeStrict m of
                      Left  e -> throwError (PutJSON e)
                      Right a -> pure a

putJSON :: (ToJSON a, Schm :> es) => a -> Eff es ()
putJSON obj = withConnection (`sendTextData` (encode obj))

handoff :: Fallible es
        => (Schm.Brief -> Schm.Prompt)
        -> Eff es ()

handoff tf = fmap tf getJSON >>= putJSON

recvRes :: Fallible es => Eff es Schm.Res
recvRes = getJSON >>= \case
        Schm.Error e -> throwError (Failure e)
        res          -> pure res

recvPinStates :: Fallible es => Eff es [Schm.Pin]
recvPinStates = recvRes >>= \case
        Schm.Res.PinStates p -> pure p
        _                    -> throwError (ObjDiff "expected pin states")

recvPinState :: Fallible es => Eff es Bool
recvPinState = recvRes >>= \case
        Schm.Res.PinState b -> pure b
        _                   -> throwError (ObjDiff "expected pin state")

recvAck :: Fallible es => Eff es ()
recvAck = recvRes >>= \case
        Schm.Ack -> pure ()
        _        -> throwError (ObjDiff "expected ack")

modifyPins :: Fallible es => [Schm.Pin] -> Eff es ()
modifyPins pins = putJSON (Schm.Req.ModifyPins pins) >> recvAck

pinStates :: Fallible es => Eff es [Schm.Pin]
pinStates = putJSON Schm.Req.PinStates >> recvPinStates

pinState :: Fallible es => Word8 -> Eff es Bool
pinState pin = putJSON (Schm.Req.PinState pin) >> recvPinState

wait :: Schm :> es => Int -> Eff es ()
wait i = unsafeEff_ $ threadDelay (i * 10)

type Host = String
type Port = Int

runApp :: IOE :> es
       => Host
       -> Port
       -> (Schm.Brief -> Schm.Prompt)
       -> Eff (Schm : Error SchmError : es) a
       -> Eff es (Either (CallStack, SchmError) a)

runApp h p f m = runError $ runSchm h p m'
        where
                m' = handoff f >> m <* closing

runSchm :: IOE :> es => Host -> Port -> Eff (Schm : es) a -> Eff es a
runSchm h p m = unsafeEff lift
        where
                lift env = run $ \c -> unEff (app c) env
                run      = runClient h p mempty
                app c    = mf
                  where
                          mf = evalStaticRep (Schm sm) m
                          sm = Schema c

gpios :: Fallible es => (Int -> [Eff (Pin : es) Int]) -> Eff es ()
gpios f = do
        pins <- pinStates

        let len = length pins
        let arr = f len
        let f e = do
                    (a, pins') <- runPin pins e;
                    modifyPins pins' >> wait a

        void $ traverse f arr

pinsTo :: Fallible es => Bool -> Eff es ()
pinsTo b = gpio $ \i -> mapM_ (`pin` b) [0..(fromIntegral i)]

pinsHi :: Fallible es => Eff es ()
pinsHi = pinsTo True

pinsLo :: Fallible es => Eff es ()
pinsLo = pinsTo False

gpio :: Fallible es => (Int -> Eff (Pin : es) a) -> Eff es a
gpio f = do
        pins       <- pinStates
        (a, pins') <- runPin pins $ f (length pins)
        modifyPins pins' $> a
