{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Types where

--------------------------------------------------------------------------------
import           Control.DeepSeq   (NFData)
import           Control.Lens      (makeLenses)
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           GHC.Generics
--------------------------------------------------------------------------------

myAesonSettings :: Options
myAesonSettings = aesonPrefix snakeCase

--------------------------------------------------------------------------------

data Context = Context ()
  deriving (Show, Generic)

makeLenses ''Context

instance ToJSON Context where
   toJSON = genericToJSON myAesonSettings
instance FromJSON Context where
   parseJSON = genericParseJSON myAesonSettings

--------------------------------------------------------------------------------

data Event a
  = Event { _eventPayload :: a
          , _eventContext :: Context
          } deriving (Show, Functor, Generic)

makeLenses ''Event

instance ToJSON a => ToJSON (Event a) where
   toJSON = genericToJSON myAesonSettings
instance FromJSON a => FromJSON (Event a) where
   parseJSON = genericParseJSON myAesonSettings

--------------------------------------------------------------------------------

newtype LambdaFunction payload err m ret
  = LambdaFunction (Event payload -> m (Either err ret))

--------------------------------------------------------------------------------

newtype UID
  = UID Integer

instance ToJSON UID where
  toJSON (UID a) = toJSON a
instance FromJSON UID where
  parseJSON = fmap UID . parseJSON

--------------------------------------------------------------------------------

data WithUID a
  = WithUID UID a
  deriving (Generic)

instance ToJSON a => ToJSON (WithUID a) where
  toJSON (WithUID i a)
    = object [ ("uid", toJSON i), ("contents", toJSON a) ]

instance FromJSON a => FromJSON (WithUID a) where
  parseJSON (Object v) = WithUID <$> v .: "uid" <*> v .: "contents"
  parseJSON _          = fail "WithUID: expected an object"

--------------------------------------------------------------------------------

data Answer
  = Success Value
  | Fail Value
  | GotException String
  | DecodeError String
  deriving Generic

instance ToJSON Answer where
  toJSON = genericToJSON myAesonSettings

--------------------------------------------------------------------------------

data In
  = InEvent (WithUID Value)
  deriving Generic

instance FromJSON In where
   parseJSON = genericParseJSON myAesonSettings

data Out
  = OutAnswer (WithUID Answer)
  deriving Generic

instance ToJSON Out where
   toJSON = genericToJSON myAesonSettings

--------------------------------------------------------------------------------
