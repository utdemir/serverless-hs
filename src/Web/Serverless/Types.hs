{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Serverless.Types
  ( module Web.Serverless.Types
  , module Web.Serverless.Types.Args
  )
  where

--------------------------------------------------------------------------------
import           Control.DeepSeq           (NFData)
import           Control.Exception.Safe
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           GHC.Generics
--------------------------------------------------------------------------------
import           Web.Serverless.Types.Args
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

data LambdaFunction payload err m ret where
  LambdaFunction :: ( FromJSON (Event payload), ToJSON ret, ToJSON err
                    , NFData ret, NFData err
                    )
                 => (Event payload -> m (Either err ret))
                 -> LambdaFunction payload err m ret


