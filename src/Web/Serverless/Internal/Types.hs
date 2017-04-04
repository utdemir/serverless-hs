{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}

module Web.Serverless.Internal.Types
  ( module Web.Serverless.Internal.Types
  , module Web.Serverless.Internal.Types.Args
  )
  where

--------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           GHC.Generics
--------------------------------------------------------------------------------
import           Web.Serverless.Internal.Types.Args
--------------------------------------------------------------------------------

aesonSettings :: Options
aesonSettings = aesonPrefix snakeCase

--------------------------------------------------------------------------------

data Context = Context ()
  deriving (Show, Generic)

instance FromJSON Context where
   parseJSON = return . const (Context ())

--------------------------------------------------------------------------------

data Event a
  = Event { _eventPayload :: a
          , _eventContext :: Context
          } deriving (Show, Functor, Generic)

instance FromJSON a => FromJSON (Event a) where
   parseJSON = genericParseJSON aesonSettings

--------------------------------------------------------------------------------

data LambdaFunction payload err ret where
  LambdaFunction :: (FromJSON payload, ToJSON ret, ToJSON err)
                 => (Event payload -> IO (Either err ret))
                 -> LambdaFunction payload err ret
