{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Lib where

--------------------------------------------------------------------------------
import           Control.Concurrent            (forkIO)
import           Control.Concurrent.Chan.Class
import           Control.Concurrent.Chan.Split
import           Control.DeepSeq               (NFData)
import           Control.Exception.Safe
import Control.Concurrent.Async
import           Control.Monad
import           Data.Aeson                    (FromJSON (..), ToJSON (..),
                                                Value, encode)
import           Data.Aeson.Internal           (IResult (..), ifromJSON)
import Data.Bool
import           Data.Aeson.Parser             (json')
import qualified Data.Attoparsec.ByteString    as A
import Debug.Trace
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import           Data.Functor
import           Data.Void
import Data.String
import           System.IO
--------------------------------------------------------------------------------
import           Types
--------------------------------------------------------------------------------

apply :: LambdaFunction payload err m ret -> Event payload -> m (Either err ret)
apply (LambdaFunction f) ev = f ev

--------------------------------------------------------------------------------

applyJSON :: ( FromJSON (Event payload), ToJSON ret, ToJSON err
             , NFData ret, NFData err
             )
          => LambdaFunction payload err IO ret -> Value -> IO Answer
applyJSON (LambdaFunction f) val =
  case ifromJSON val of
    err@(IError _ _) -> return $ DecodeError (show $ err $> ())
    ISuccess xs -> tryAnyDeep (f xs) >>= \case
      Left ex -> return . GotException $ show (ex :: SomeException)
      Right (Left err)  -> return $ Fail (toJSON err)
      Right (Right ret) -> return $ Success (toJSON ret)

server :: ( FromJSON (Event payload), ToJSON ret, ToJSON err
          , NFData ret , NFData err
          )
       => OutChan In -> InChan Out
       -> LambdaFunction payload err IO ret
       -> IO Void
server outChan inChan fun = do
  InEvent (WithUID uid payload) <- readChan outChan
  _ <- forkIO $
    applyJSON fun payload >>= writeChan inChan . OutAnswer . WithUID uid
  server outChan inChan fun

fileSource :: FromJSON a => FilePath -> InChan a -> IO Void
fileSource fp chan =
  withFile fp ReadMode $ \h ->
    let go :: BS.ByteString -> IO Void
        go rest = do
          A.parseWith (BS.hGetSome h 1024) (json' <* fromString "\n") rest
            >>= \case
              err@(A.Fail _ _ _) -> throwIO . userError $ (show $ err $> ())
              A.Partial _        -> throwIO . userError $ "not enough input"
              A.Done newRest ret -> do
                case ifromJSON ret of
                  err@(IError _ _) ->
                    hPutStrLn stderr $
                      "Got invalid json, ignoring: " ++ (show $ err $> ())
                  ISuccess xs -> writeChan chan xs
                go newRest
    in go BS.empty

fileSink :: ToJSON a => FilePath -> OutChan a -> IO Void
fileSink fp chan =
  withFile fp WriteMode $ \h ->
    forever $ readChan chan
          >>= BS.hPut h . BL.toStrict . encode
          >> hPutChar h '\n' >> hFlush h

fileServer :: ( FromJSON (Event payload), ToJSON ret, ToJSON err
              , NFData ret, NFData err
              )
           => FilePath -> FilePath
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
