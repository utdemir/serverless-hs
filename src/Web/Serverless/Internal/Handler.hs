{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Web.Serverless.Internal.Handler where

--------------------------------------------------------------------------------
import           Control.Exception.Safe
import Data.Void
import           Data.Aeson                    hiding (Success)
import           Data.Aeson.Internal           (IResult (..), ifromJSON)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import           Data.Functor
import           GHC.Generics
import           Network.HTTP.Types
--------------------------------------------------------------------------------
import Web.Serverless.Internal.SocketApp
import           Web.Serverless.Internal.Types
--------------------------------------------------------------------------------

data FailType err
  = Fail err
  | GotException String
  deriving Generic

instance ToJSON err => ToJSON (FailType err) where
  toJSON = genericToJSON aesonSettings

data Answer err ret
  = Success ret
  | Failure (FailType err)
  deriving Generic

instance (ToJSON err, ToJSON ret) => ToJSON (Answer err ret) where
  toJSON = genericToJSON aesonSettings

--------------------------------------------------------------------------------

data In payload
  = InEvent payload
  deriving Generic

instance FromJSON a => FromJSON (In a) where
   parseJSON = genericParseJSON aesonSettings

data Out err ret
  = OutAnswer (Answer err ret)
  deriving Generic

instance (ToJSON err, ToJSON ret) => ToJSON (Out err ret) where
   toJSON = genericToJSON aesonSettings

--------------------------------------------------------------------------------

run :: Int -> LambdaFunction payload err ret -> IO Void
run port (LambdaFunction fun) = runSocketApp port $ \val ->
  try (fun $ Event val (Context ())) >>= \case
    Left ex -> return . Failure . GotException $ show (ex :: SomeException)
    Right (Left err)  -> return . Failure $ Fail err
    Right (Right ret) -> return $ Success ret

--------------------------------------------------------------------------------

decodeJSON :: BS.ByteString -> Either String Value
decodeJSON = eitherDecodeStrict

decodeValue :: FromJSON a => Value -> Either String a
decodeValue = resToStr . ifromJSON
  where
    resToStr :: IResult a -> Either String a
    resToStr (ISuccess a) = Right $ a
    resToStr err          = Left  $ show (err $> ()) 
      
encodeJSON :: ToJSON a => a -> Value
encodeJSON = toJSON

encodeValue :: Value -> BS.ByteString
encodeValue = BL.toStrict . encode
