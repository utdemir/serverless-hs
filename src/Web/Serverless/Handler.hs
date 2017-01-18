{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Web.Serverless.Handler where

--------------------------------------------------------------------------------
import           Control.Exception.Safe

import           Data.Aeson                    hiding (Success)
import           Data.Aeson.Internal           (IResult (..), ifromJSON)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import           Data.Functor
import           Data.Void
import           GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
--------------------------------------------------------------------------------
import           Web.Serverless.Types
--------------------------------------------------------------------------------

data FailType err
  = Fail err
  | GotException String
  | DecodeError String
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

server :: LambdaFunction payload err IO ret -> Int -> IO Void
server fun port
  = run port (application fun)
      >> error "invariant violation: warp terminated"
  

application :: LambdaFunction payload err IO ret -> Application
application fun@(LambdaFunction _) req resp
  = lazyRequestBody req
      >>= applyJSON fun
      >>= resp . responseLBS status200 [] . encode

--------------------------------------------------------------------------------

apply :: LambdaFunction payload err m ret -> Event payload -> m (Either err ret)
apply (LambdaFunction f) ev = f ev

--------------------------------------------------------------------------------

applyJSON :: LambdaFunction payload err IO ret
          -> BL.ByteString
          -> IO (Answer err ret)
applyJSON (LambdaFunction f) val =
  case eitherDecode val of
    Left err -> return $ Failure (DecodeError err)
    Right xs -> tryAnyDeep (f xs) >>= \case
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
