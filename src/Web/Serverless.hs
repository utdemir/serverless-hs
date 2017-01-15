{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Serverless
  ( LambdaFunction (LambdaFunction)
  , Event(..)
  , Context(..)
  , serverlessMain
  ) where

--------------------------------------------------------------------------------
import Options.Generic
import Data.Void
--------------------------------------------------------------------------------
import Web.Serverless.Types
import Web.Serverless.Handler
--------------------------------------------------------------------------------

serverlessMain :: LambdaFunction payload err IO ret -> IO ()
serverlessMain fun = do
  Args{..} <- getRecord "Serverless Haskell"
  absurd <$> server fun argsPort
  

