{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Web.Serverless
  ( LambdaFunction (LambdaFunction)
  , Event(..)
  , Context(..)
  , serverlessMain
  , function
  , resource
  , Handle (handleToText)
  , handleRef
  ) where

--------------------------------------------------------------------------------
import Options.Generic
import System.Environment
import qualified Data.Text as T
import System.IO (stderr, hPutStrLn)
import Data.Bifunctor
import Data.Monoid
import System.Exit
--------------------------------------------------------------------------------
import Web.Serverless.Internal.Types
import Web.Serverless.Internal.Handler
import Web.Serverless.Internal.Spec
import Web.Serverless.Internal.Constants
--------------------------------------------------------------------------------

serverlessMain :: SpecM () -> IO ()
serverlessMain spec = do
  Args{..} <- getRecord "Serverless Haskell"
  env <- map (bimap T.pack T.pack) <$> getEnvironment

  functionId <- case envFunctionId `lookup` env of
    Just fid -> return fid
    Nothing -> do
      hPutStrLn stderr . T.unpack $ "Missing " <> envFunctionId
      exitWith (ExitFailure 1)
  
  if functionId == "meta"
    then run argsPort (metaSpec spec) 
    else case lookup envFunctionId <$> executeSpecM (run argsPort) env spec of
      Left err -> error err
      Right Nothing -> error "invalid function id"
      Right (Just a) -> a
      
  return ()
