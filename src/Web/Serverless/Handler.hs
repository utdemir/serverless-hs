{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Web.Serverless.Handler where

--------------------------------------------------------------------------------
import           Control.Concurrent            (forkIO)
import           Control.Concurrent.Chan.Class
import           Control.Concurrent.Chan.Split
import           Control.Exception.Safe
import           Control.Concurrent.Async
import           Control.Monad
import           Data.Aeson                    hiding (Success)
import           Data.Aeson.Internal           (IResult (..), ifromJSON)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import           Data.Functor
import           Data.Void
import           GHC.Generics
import           System.IO
--------------------------------------------------------------------------------
import           Web.Serverless.Types
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
  toJSON = genericToJSON aesonSettings

--------------------------------------------------------------------------------

data In
  = InEvent (WithUID Value)
  deriving Generic

instance FromJSON In where
   parseJSON = genericParseJSON aesonSettings

data Out
  = OutAnswer (WithUID Answer)
  deriving Generic

instance ToJSON Out where
   toJSON = genericToJSON aesonSettings

--------------------------------------------------------------------------------

apply :: LambdaFunction payload err m ret -> Event payload -> m (Either err ret)
apply (LambdaFunction f) ev = f ev

--------------------------------------------------------------------------------

applyJSON :: LambdaFunction payload err IO ret -> Value -> IO Answer
applyJSON (LambdaFunction f) val =
  case ifromJSON val of
    err@(IError _ _) -> return $ DecodeError (show $ err $> ())
    ISuccess xs -> tryAnyDeep (f xs) >>= \case
      Left ex -> return . GotException $ show (ex :: SomeException)
      Right (Left err)  -> return $ Fail (toJSON err)
      Right (Right ret) -> return $ Success (toJSON ret)

server :: OutChan In -> InChan Out
       -> LambdaFunction payload err IO ret
       -> IO Void
server outChan inChan fun = do
  InEvent (WithUID uid payload) <- readChan outChan
  _ <- forkIO $
    applyJSON fun payload >>= writeChan inChan . OutAnswer . WithUID uid
  server outChan inChan fun

fileSource :: FromJSON a => FilePath -> InChan a -> IO Void
fileSource fp chan =
  withFile fp ReadMode $ \h -> forever $ do
    line <- BS.hGetLine h
    case decodeJSON line >>= decodeValue of
      Left err -> hPutStrLn stderr $ "Got invalid json, ignoring: " ++ err
      Right xs -> writeChan chan xs

fileSink :: ToJSON a => FilePath -> OutChan a -> IO Void
fileSink fp chan =
  withFile fp WriteMode $ \h -> forever $
    readChan chan
     >>= BS.hPut h . encodeValue . encodeJSON
     >>  hPutChar h '\n' >> hFlush h

fileServer :: FilePath -> FilePath
           -> LambdaFunction payload err IO ret
           -> IO Void
fileServer source sink fun = do
  (sourceIn, sourceOut) <- newSplitChan :: IO (InChan In , OutChan In)
  (sinkIn  , sinkOut  ) <- newSplitChan :: IO (InChan Out, OutChan Out)

  asyncs <- sequence [ async $ fileSource source sourceIn
                     , async $ fileSink sink sinkOut
                     , async $ server sourceOut sinkIn fun
                     ]

  snd <$> waitAnyCancel asyncs

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
