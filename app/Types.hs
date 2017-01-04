{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE TypeInType     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving     #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( module Types
  , module Types.Args
  , module Control.Monad.Trans.Monologue
  ) where

--------------------------------------------------------------------------------
import           Control.Exception.Safe
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as T
import           GHC.Generics
import qualified Data.ByteString as BS
import           Network.AWS
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Monologue
import Data.Maybe
import Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
--------------------------------------------------------------------------------
import           Types.Args
--------------------------------------------------------------------------------

aesonSettings :: Options
aesonSettings = aesonPrefix snakeCase

--------------------------------------------------------------------------------

newtype M a = M (MonologueT (AWST (ResourceT IO)) a)
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadThrow, MonadCatch, MonadMonologue
           )
instance MonadAWS M where
  liftAWS = M . MonologueT . liftAWS
  
runM :: M a -> IO a
runM (M m) = do
  env <- newEnv Discover
  runResourceT . runAWS env . runMonologueT $ m

--------------------------------------------------------------------------------

newtype HandlerJS     = HandlerJS     BS.ByteString
newtype HsMain        = HsMain        BS.ByteString
newtype DeploymentZip = DeploymentZip BS.ByteString

--------------------------------------------------------------------------------

data AWSResType
  = AWSBucket | AWSStack

data AWSResNameType
  = AWSName | AWSId

newtype AWSRes
  (res :: AWSResType)
  (ty  :: AWSResNameType)
  = AWSRes { fromAWSRes :: Text }
  deriving (Show, Eq, Ord)

data AWSStackResult
  = AWSStackResult
      { _awsStackId     :: AWSRes 'AWSStack  'AWSId
      , _awsStackBucket :: AWSRes 'AWSBucket 'AWSId
      }

--------------------------------------------------------------------------------

data Config'
  = Config' { config'Region    :: Maybe Region
            , config'StackName :: Text
            }
  deriving (Show, Generic)

instance FromJSON Config' where
   parseJSON = genericParseJSON aesonSettings

data Config
  = Config { configRegion :: Region
           , configStackName :: AWSRes 'AWSStack 'AWSName
           }
  deriving (Show)

config'ToConfig :: Config' -> Config
config'ToConfig Config'{..}
  = Config (fromMaybe NorthVirginia config'Region)
           (AWSRes config'StackName)

instance FromJSON Config where
  parseJSON = fmap config'ToConfig . parseJSON

--------------------------------------------------------------------------------

data DeployException
  = InvalidFile FilePath Text
  | AWSError Text Text
  deriving Show

instance Exception DeployException where
  displayException (InvalidFile fpath desc)
    = T.unpack $ "Error reading " <> T.pack fpath <> ": " <> desc
  displayException (AWSError hdr desc)
    = T.unpack $ hdr <> ": " <> desc

