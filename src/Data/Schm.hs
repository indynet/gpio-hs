module Data.Schm where

import Data.Aeson

import Data.Schm.Prompt (Prompt(..))
import Data.Schm.Brief  (Brief(..))
import Data.Schm.Req    (Req)
import Data.Schm.Res    (Res)
import Data.Schm.Pin    (Pin)

instance FromJSON Prompt
instance ToJSON   Prompt
instance FromJSON Brief
instance ToJSON   Brief
instance FromJSON Pin
instance ToJSON   Pin
instance FromJSON Req
instance ToJSON   Req
instance FromJSON Res
instance ToJSON   Res

promptAll :: String -> Brief -> Prompt
promptAll name (Brief pins) = Prompt name pins
