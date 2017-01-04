{-# LANGUAGE OverloadedStrings #-}

module Web.Serverless
  ( LambdaFunction (LambdaFunction)
  , Event(..)
  , Context(..)
  , serverlessMain
  ) where

--------------------------------------------------------------------------------
import Options.Generic
--------------------------------------------------------------------------------
import Web.Serverless.Types
import Web.Serverless.Handler
--------------------------------------------------------------------------------

serverlessMain :: LambdaFunction payload err m ret -> IO ()
serverlessMain fun = do
  x <- getRecord "Serverless Haskell"
  print (x :: Args)

