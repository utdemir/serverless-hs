{-# LANGUAGE LambdaCase #-}

module Web.Serverless.Internal.SocketApp
  ( runSocketApp
  , PortNumber
  ) where
 
--------------------------------------------------------------------------------
import Data.Void
import Data.Aeson
import Data.Aeson.Internal
import Data.Aeson.Parser
import Data.Functor
import Network.Socket (Socket)
import Network.Simple.TCP (HostPreference(HostAny), serve)
import Data.Attoparsec.ByteString
import Network.Socket.ByteString
import Control.Exception.Safe
import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
--------------------------------------------------------------------------------

type SocketApp a b = a -> IO b
type PortNumber = Int

runSocketApp :: (FromJSON a, ToJSON b) => Int -> SocketApp a b -> IO Void
runSocketApp port app
  = serve HostAny (show port) (handler app . fst)
      $> error "runSocketApp: Terminated." 

handler :: (FromJSON a, ToJSON b) => SocketApp a b -> Socket -> IO ()
handler app sock
  = eitherResult <$> parseWith (recv sock 1024) value' BS.empty >>= \case
      Left err -> hPutStrLn stderr $ "Invalid json received: " ++ err
      Right val -> case ifromJSON val of
        IError path str -> hPutStrLn stderr $ "Error parsing json: " ++ formatError path str 
        ISuccess obj -> app obj >>= void . send sock . BL.toStrict . encode
      
